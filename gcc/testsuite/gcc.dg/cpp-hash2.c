/* Test for erroneously thinking comments are token-pastes.
   From XFree86 4.0.  */
/* { dg-do preprocess } */
/* { dg-options "-traditional" } */

#ifndef foo
#define foo	/**/
#endif

#ifndef foo
#define foo	/* as nothing */
#endif

/* { dg-bogus "(start|end) of macro" "/**/ at end" { target *-*-* } 7 } */
/* { dg-bogus "(start|end) of macro" "comment at end" { target *-*-* } 11 } */
