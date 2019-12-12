/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector signed short
vllebrzh (const signed short *a)
{
  return vec_revb (vec_insert_and_zero (a));
}

/* { dg-final { scan-assembler-times "vllebrzh.*\n\tvllebrzh.*vec_insert_and_zero_bswapv8hi" 1 } } */

vector signed int
vllebrzf (const signed int *a)
{
  return vec_revb (vec_insert_and_zero (a));
}

/* { dg-final { scan-assembler-times "vllebrzf.*\n\tvllebrzf.*vec_insert_and_zero_bswapv4si" 1 } } */

vector signed long long
vllebrzg (const signed long long *a)
{
  return vec_revb (vec_insert_and_zero (a));
}

/* { dg-final { scan-assembler-times "vllebrzg.*\n\tvllebrzg.*vec_insert_and_zero_bswapv2di" 1 } } */
