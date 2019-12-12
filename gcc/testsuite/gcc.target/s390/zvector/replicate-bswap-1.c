/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector signed short
vlbrreph (const signed short *a)
{
  return vec_revb (vec_splats (*a));
}

/* { dg-final { scan-assembler-times "vlbrreph.*\n\tvlbrreph.*vec_splats_bswap_vecv8hi" 1 } } */

vector signed int
vlbrrepf (const signed int *a)
{
  return vec_revb (vec_splats (*a));
}

/* { dg-final { scan-assembler-times "vlbrrepf.*\n\tvlbrrepf.*vec_splats_bswap_vecv4si" 1 } } */

vector signed long long
vlbrrepg (const signed long long *a)
{
  return vec_revb (vec_splats (*a));
}

/* { dg-final { scan-assembler-times "vlbrrepg.*\n\tvlbrrepg.*vec_splats_bswap_vecv2di" 1 } } */
