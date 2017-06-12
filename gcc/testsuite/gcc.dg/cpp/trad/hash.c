/* Test for erroneously thinking comments are token-pastes.
   From XFree86 4.0.  */
/* { dg-do preprocess } */

#ifndef foo
#define foo	/**/
#endif
/* { dg-bogus "(start|end) of macro" "paste at end" { target *-*-* } .-1 } */

#ifndef foo
#define foo	/* as nothing */
#endif
/* { dg-bogus "(start|end) of macro" "comment at end" { target *-*-* } .-1 } */
