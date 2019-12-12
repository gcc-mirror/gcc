/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector unsigned short
vlbrreph (const unsigned short *a)
{
  return vec_splats (__builtin_bswap16 (*a));
}

/* { dg-final { scan-assembler-times "vlbrreph.*\n\tvlbrreph.*vec_splats_bswap_elemv8hi" 1 } } */

vector unsigned int
vlbrrepf (const unsigned int *a)
{
  return vec_splats (__builtin_bswap32 (*a));
}

/* { dg-final { scan-assembler-times "vlbrrepf.*\n\tvlbrrepf.*vec_splats_bswap_elemv4si" 1 } } */

vector unsigned long long
vlbrrepg (const unsigned long long *a)
{
  return vec_splats ((unsigned long long)__builtin_bswap64 (*a));
}

/* { dg-final { scan-assembler-times "vlbrrepg.*\n\tvlbrrepg.*vec_splats_bswap_elemv2di" 1 } } */
