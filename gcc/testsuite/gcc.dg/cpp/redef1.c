/* Test for redefining macros with insignificant (i.e. whitespace)
   differences.  */

/* { dg-do preprocess } */

#define foo bar
#define /* x */ foo /* x */ bar /* x */

#define quux(thud) a one and a thud and a two
#define /**/ quux( thud ) /**/ a one and a /**/ thud /**/ and /**/ a two
#define quux(thud) a one	and a thud and a 	two /* bah */

/* { dg-bogus "redefined" "foo redefined"	{ target *-*-* } 7 } */
/* { dg-bogus "redefined" "quux redefined"	{ target *-*-* } 10 } */
/* { dg-bogus "redefined" "quux redefined"	{ target *-*-* } 11 } */

/* { dg-bogus "previous def" "foo prev def"	{ target *-*-* } 6 } */
/* { dg-bogus "previous def" "quux prev def"	{ target *-*-* } 9 } */
/* { dg-bogus "previous def" "quux prev def"	{ target *-*-* } 10 } */
