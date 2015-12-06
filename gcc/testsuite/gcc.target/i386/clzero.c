/* { dg-do compile } */
/* { dg-options "-O2 -mclzero" } */

/* Verify that they work in both 32bit and 64bit.  */

#include <x86intrin.h>

void
foo (void *k)
{
   _mm_clzero (k);
}

