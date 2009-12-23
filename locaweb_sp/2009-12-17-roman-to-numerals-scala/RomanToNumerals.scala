import org.specs.Specification


object DojoSpecs extends Specification {
	"roman to numerals" should {
		"convert I" in {
			romanToNumerals("I") must_== 1
		}

		"convert II" in {
			romanToNumerals("II") must_== 2
		}

		"convert III" in {
			romanToNumerals("III") must_== 3
		}

		"convert IV" in {
			romanToNumerals("IV") must_== 4
		}
		"convert V" in {
			romanToNumerals("V")  must_== 5
		}
		"convert VI" in {
			romanToNumerals("VI")  must_== 6
		}

		"convert VII" in {
			romanToNumerals("VII")  must_== 7
		}

		"convert VIII" in {
			romanToNumerals("VIII")  must_== 8
		}

		"convert IX" in {
			romanToNumerals("IX")  must_== 9
		}

		"convert X" in {
			romanToNumerals("X")  must_== 10
		}

		"convert XI" in {
			romanToNumerals("XI")  must_== 11
		}
		"convert XII" in {
			romanToNumerals("XII")  must_== 12
		}
		"convert XIII" in {
			romanToNumerals("XIII")  must_== 13
		}
		"convert XIV" in {
			romanToNumerals("XIV")  must_== 14
		}
		"convert XIX" in {
			romanToNumerals("XIX") must_== 19 
		}
		"convert XXIX" in {
		  romanToNumerals("XXIX") must_== 29
		}
		"convert LIV" in {
		  romanToNumerals("LIV") must_==54
		}
		"convert MCMLXXXIII" in {
			romanToNumerals("MCMLXXXIII") must_== 1983
		}
		"convert MMX" in {
			romanToNumerals("MMX") must_== 2010
		}
		"convert MMDC" in {
		  romanToNumerals("MMDC") must_== 2600
		}
  
	}

	val char_to_num = Map(
			'I' -> 1,
			'V' -> 5,
			'X' -> 10,
			'L' -> 50,
			'C' -> 100,
			'D' -> 500,
			'M' -> 1000
	)

	def romanToNumerals(number: String) = {
		def romanToNumeralsList(numberList: List[java.lang.Integer]):java.lang.Integer = {
			numberList match{
				case first::second::tail => {
					if(first.intValue < second.intValue)
						romanToNumeralsList(second::tail).intValue - first.intValue 
					else 
						first.intValue + romanToNumeralsList(second::tail).intValue
				}
				case first::tail => first
				case _ => 0
			}
		}
		romanToNumeralsList(number.toList.map(a => char_to_num(a)))
	}
}