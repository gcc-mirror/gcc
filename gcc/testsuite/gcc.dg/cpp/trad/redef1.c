/* Test for redefining traditional macros with insignificant
   (i.e. whitespace) differences.  */

/* { dg-do preprocess } */


#define foo bar
#define /* x */ foo /* x */ bar /* x */

#define quux(thud) a one and a thud and a two
#define /**/ quux( thud ) /**/ a one and a /**/ thud /**/ and /**/ a two
#define quux(thud) a one	and a thud and a 	two /* bah */

#define f(x, y)x "x  y z"  y
#define f(x, y) x  "x  y z" y

#define baz() whiz bang
#define baz() whiz  bang

#define g foo
#undef g
#define g

/* { dg-bogus "redefined" "foo redefined"	{ target *-*-* } 8 } */
/* { dg-bogus "redefined" "quux redefined"	{ target *-*-* } 11 } */
/* { dg-bogus "redefined" "quux redefined"	{ target *-*-* } 12 } */
/* { dg-bogus "redefined" "f redefined"		{ target *-*-* } 15 } */
/* { dg-bogus "redefined" "baz redefined"	{ target *-*-* } 18 } */
/* { dg-bogus "redefined" "g redefined"		{ target *-*-* } 22 } */

/* { dg-bogus "previous def" "foo prev def"	{ target *-*-* } 7 } */
/* { dg-bogus "previous def" "quux prev def"	{ target *-*-* } 10 } */
/* { dg-bogus "previous def" "quux prev def"	{ target *-*-* } 11 } */
/* { dg-bogus "previous def" "f prev def"	{ target *-*-* } 14 } */
/* { dg-bogus "previous def" "baz prev def"	{ target *-*-* } 17 } */
/* { dg-bogus "previous def" "g prev def"	{ target *-*-* } 20 } */
