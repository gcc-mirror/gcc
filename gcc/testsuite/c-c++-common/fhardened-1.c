/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -O" } */

#if !defined(__SSP_STRONG__) && !defined(__hppa__)
# error "-fstack-protector-strong not enabled"
#endif

#if _FORTIFY_SOURCE < 2
# error "_FORTIFY_SOURCE not enabled"
#endif

#ifndef _GLIBCXX_ASSERTIONS
# error "_GLIBCXX_ASSERTIONS not enabled"
#endif
