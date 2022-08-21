/* { dg-do compile } */
/* { dg-options "-O3 -march=skylake-avx512" } */
/* { dg-additional-options "-fno-PIE" { target ia32 } } */

#include<immintrin.h>

__m256i
foo ()
{
  return _mm256_set1_epi16 (12);
}

/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+%r\[^\n\]*, %ymm\[0-9\]+" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "vpbroadcastq\[\\t \]+\[^\n\]*, %ymm\[0-9\]+" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "vmovdqa" } } */
/* { dg-final { scan-assembler-not "vzeroupper" } } */
