/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector -fno-asynchronous-unwind-tables -dp" } */

#include <vecintrin.h>

void
vstebrh (signed short *a, vector signed short b)
{
  *a = vec_revb (b)[1];
}

/* { dg-final { scan-assembler-times "vstebrh.*\n\tvstebrh.*vec_extract_bswap_vecv8hi" 1 } } */

void
vstebrf (int *a, vector int b)
{
  *a = vec_revb (b)[1];
}

/* { dg-final { scan-assembler-times "vstebrf.*\n\tvstebrf.*vec_extract_bswap_vecv4si" 1 } } */

void
vstebrg (long long *a, vector long long b)
{
  *a = vec_revb (b)[1];
}

/* { dg-final { scan-assembler-times "vstebrg.*\n\tvstebrg.*vec_extract_bswap_vecv2di" 1 } } */
