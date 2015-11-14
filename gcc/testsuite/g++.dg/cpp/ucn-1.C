// http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2007/n2170.html
// { dg-do compile { target c++11 } }
// { dg-options "" }

int main()
{
  "\u0041";			// 'A' UCN is OK in string literal
  '\u0041';			// also OK in character literal

  int c\u0041c;		// { dg-error "not valid in an identifier" }
		// $ is OK on most targets; not part of basic source char set
  int c\u0024c;	// { dg-error "not valid in an identifier" "" { target { powerpc-ibm-aix* } } }

  U"\uD800";		  // { dg-error "not a valid universal character" }
}
