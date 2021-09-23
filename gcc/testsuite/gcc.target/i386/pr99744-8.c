/* { dg-do compile } */
/* { dg-options "-O -Wno-psabi" } */

#include <x86intrin.h>

__attribute__((target ("no-avx")))
void
foo (__m256 *x)
{
  x[0] = _mm256_sub_ps (x[1], x[2]);
}

/* { dg-error "target specific option mismatch" "" { target *-*-* } 0 } */
