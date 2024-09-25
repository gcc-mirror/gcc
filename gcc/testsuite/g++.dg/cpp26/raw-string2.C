// C++26 P2558R2 - Add @, $, and ` to the basic character set
// { dg-do compile { target { ! { avr*-*-* mmix*-*-* *-*-aix* } } } }
// { dg-options "" }

int a$b;
int a\u0024c;		// { dg-error "universal character \\\\u0024 is not valid in an identifier" "" { target c++26 } }
int a\U00000024d;	// { dg-error "universal character \\\\U00000024 is not valid in an identifier" "" { target c++26 } }
