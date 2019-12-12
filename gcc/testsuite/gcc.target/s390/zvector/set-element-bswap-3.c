/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

vector signed short
vlebrh (const signed short *a, vector signed short b)
{
  b[1] = __builtin_bswap16 (*a);
  return b;
}

/* { dg-final { scan-assembler-times "vlebrh.*\n\tvlebrh.*vec_set_bswap_elemv8hi" 1 } } */

vector signed int
vlebrf (const signed int *a, vector signed int b)
{
  b[1] = __builtin_bswap32 (*a);
  return b;
}

/* { dg-final { scan-assembler-times "vlebrf.*\n\tvlebrf.*vec_set_bswap_elemv4si" 1 } } */

vector signed long long
vlebrg (const signed long long *a, vector signed long long b)
{
  b[1] = __builtin_bswap64 (*a);
  return b;
}

/* { dg-final { scan-assembler-times "vlebrg.*\n\tvlebrg.*vec_set_bswap_elemv2di" 1 } } */
