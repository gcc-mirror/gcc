/* Test for redefining traditional macros with insignificant
   (i.e. whitespace) differences.  */

/* { dg-do preprocess } */


#define foo bar
#define /* x */ foo /* x */ bar /* x */
/* { dg-bogus "redefined" "foo redefined"	{ target *-*-* } .-1 } */
/* { dg-bogus "previous def" "foo prev def"	{ target *-*-* } .-3 } */

#define quux(thud) a one and a thud and a two /* { dg-line quux_def_1 } */
#define /**/ quux( thud ) /**/ a one and a /**/ thud /**/ and /**/ a two /* { dg-line quux_def_2 } */
/* { dg-bogus "redefined" "quux redefined"	{ target *-*-* } quux_def_2 } */
/* { dg-bogus "previous def" "quux prev def"	{ target *-*-* } quux_def_1 } */

#define quux(thud) a one	and a thud and a 	two /* bah */ /* { dg-line quux_def_3 } */
/* { dg-bogus "redefined" "quux redefined"	{ target *-*-* } quux_def_3 } */
/* { dg-bogus "previous def" "quux prev def"	{ target *-*-* } quux_def_2 } */

#define f(x, y)x "x  y z"  y
#define f(x, y) x  "x  y z" y
/* { dg-bogus "redefined" "f redefined"		{ target *-*-* } .-1 } */
/* { dg-bogus "previous def" "f prev def"	{ target *-*-* } .-3 } */

#define baz() whiz bang
#define baz() whiz  bang
/* { dg-bogus "redefined" "baz redefined"	{ target *-*-* } .-1 } */
/* { dg-bogus "previous def" "baz prev def"	{ target *-*-* } .-3 } */

#define g foo
#undef g
#define g
/* { dg-bogus "redefined" "g redefined"		{ target *-*-* } .-1 } */
/* { dg-bogus "previous def" "g prev def"	{ target *-*-* } .-4 } */
