/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -D_FORTIFY_SOURCE=1" } */

#if _FORTIFY_SOURCE != 1
# error "_FORTIFY_SOURCE != 1"
#endif

#ifndef _GLIBCXX_ASSERTIONS
# error "_GLIBCXX_ASSERTIONS disabled when it should not be"
#endif

/* { dg-warning "._FORTIFY_SOURCE. is not enabled" "" { target *-*-* } 0 } */
