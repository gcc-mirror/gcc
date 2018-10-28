// written by Don Clugston:
//    http://www.digitalmars.com/d/archives/digitalmars/D/announce/4374.html
//    http://www.99-bottles-of-beer.net/language-d-1212.html

// Displays the "99 bottles of beer" song at compile time,
// using the template metaprograming facilities of D. 
// No executable is generated. No libraries are used.
// Illustrates template default values, template string value parameters,
// compile-time concatenation of constant strings, static if.

module bottles99;

template decimaldigit(int n) {
    const string decimaldigit = "0123456789"[n..n+1];
}

template itoa(ulong n)
{
    static if ( n < 10L )
	const string itoa = decimaldigit!(n);
    else
	const string itoa = itoa!( n / 10L ) ~ decimaldigit!( n % 10L );
}

template showHowMany(int n, string where, bool needcapital = false)
{
  static if ( n > 1 ) 
      const string showHowMany = itoa!(n) ~ " bottles of beer" ~ where ~ "\n";
  else static if ( n == 1 )
      const string showHowMany = "1 bottle of beer" ~ where ~ "\n";
  else static if ( needcapital )
      const string showHowMany = "No more bottles of beer" ~ where ~ "\n";
  else 
      const string showHowMany = "no more bottles of beer" ~ where ~ "\n";
}

template beer(int maxbeers, int n = maxbeers)
{
  static if ( n > 0 )
    const string beer = showHowMany!(n, " on the wall,", true)
        ~ showHowMany!(n, ".")
        ~ "Take one down and pass it around, " ~ "\n" 
        ~ showHowMany!( n - 1 , " on the wall.") 
        ~ "\n" ~ beer!(maxbeers, n - 1); // recurse for subsequent verses.
  else
    const string beer = showHowMany!(n, " on the wall,", true)
        ~ showHowMany!(n, ".")
        ~ "Go to the store and buy some more, " ~ "\n"
        ~ showHowMany!( maxbeers, " on the wall.");
}

pragma(msg, beer!(99));
