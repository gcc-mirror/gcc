/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fomit-frame-pointer -march=core2" } */

#include <mmintrin.h>

__m64 x;
__m64 y;

unsigned long long  foo(__m64 m) {
  return _mm_cvtm64_si64(_mm_add_pi32(x, y));
}

/* { dg-final { scan-assembler-times "mov" 2 { target nonpic } } } */
/* { dg-final { scan-assembler-times "mov" 4 { target { ! nonpic } } } } */
