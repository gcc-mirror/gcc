/* { dg-do compile } */
/* { dg-options "-O -mno-avx -Wno-psabi" } */

#include <x86intrin.h>

void
foo (__m256 *x)
{
  x[0] = _mm256_sub_ps (x[1], x[2]);
}

/* { dg-error "target specific option mismatch" "" { target *-*-* } 0 } */
