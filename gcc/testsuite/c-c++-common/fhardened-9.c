/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -U_FORTIFY_SOURCE -U_GLIBCXX_ASSERTIONS" } */

#if defined(_FORTIFY_SOURCE) || defined(_GLIBCXX_ASSERTIONS)
# error "hardening enabled when it should not be"
#endif

/* { dg-warning "._FORTIFY_SOURCE. is not enabled" "" { target *-*-* } 0 } */
/* { dg-warning "._GLIBCXX_ASSERTIONS. is not enabled" "" { target *-*-* } 0 } */
