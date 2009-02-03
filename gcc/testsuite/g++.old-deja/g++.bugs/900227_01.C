// { dg-do assemble  }
// { dg-options "" }
// g++ 1.37.1 bug 900227_01

// g++ allows pointer type values to be converted to integral types which are
// not actually large enough to hold the converted values.

// Section 3.3.4 of the ANSI C standard says:

//	A pointer may be converted to an integral type.  The size of the
//	integer required and the results are implementation defined.  If
//	the space provided is not long enough, the behavior is undefined.

// I believe that the only proper thing to do in such cases is to generate
// errors.  After all, if the converted value gets truncated, it is not
// likely to be useful after that.

// Furthermore, as the following example demonstrates, allowing pointers
// to be converted to integral types which are not of sufficient size to
// completely hold the converted values may cause additional troubles.

// I tried the following code on 5 different machines and it failed on
// all five (unless I also use the GNU assembler and the GNU linker).  Three
// of the five (Sun3, Sun4, and Symmetry) got link-time errors about byte
// offset overflows.  The other two (368/SystemV and AViiON) got assembly
// time errors about relocatable names used in "constant" expressions.

// keywords: casts, pointer types, integral types

// Update 2/10/95: The compiler will now compute these expressions at
// runtime.  I think this is in the spirit of the GNU compilers (jason).


int main ();

short s = (short) &main;	// { dg-error "loses precision" "lose" { xfail h8*-*-* xstormy16-*-* } }
char c = (char) &main;		// { dg-error "loses precision" "lose" }

int main () { return 0; }
