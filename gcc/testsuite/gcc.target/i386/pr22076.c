/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -mmmx -mno-sse2" } */
/* { dg-additional-options "-fno-common" { target *-*-darwin* } } */
/* { dg-additional-options "-mdynamic-no-pic" { target { ia32 && *-*-darwin* } } } */

#include <mmintrin.h>

__m64 x;

void test ()
{
  __m64 mm0 = (__m64)(__v8qi) {1,2,3,4,5,6,7,8};
  __m64 mm1 = (__m64)(__v8qi) {11,22,33,44,55,66,77,88};

  x = _mm_add_pi8 (mm0, mm1);
}

/* { dg-final { scan-assembler-times "movq" 2 } } */
/* { dg-final { scan-assembler-not "movl" { target nonpic } } } */
