/* { dg-do compile { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-fhardened -O -D_FORTIFY_SOURCE_ -D_GLIBCXX_ASSERTIONS_" } */

#ifndef _FORTIFY_SOURCE
# error "_FORTIFY_SOURCE disabled when it should not be"
#endif

#ifndef _GLIBCXX_ASSERTIONS
# error "_GLIBCXX_ASSERTIONS disabled when it should not be"
#endif
