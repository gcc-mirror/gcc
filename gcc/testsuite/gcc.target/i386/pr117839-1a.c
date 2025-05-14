/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic" } */
/* { dg-final { scan-assembler-times "xor\[a-z\]*\[\t \]*%xmm\[0-9\]\+,\[^,\]*" 1 } } */

#include <stddef.h>

void
clear_memory (void *mem1, size_t nclears1, void *mem2, size_t nclears2)
{
  size_t *d1 = (size_t *) mem1;

  *(d1 + 0) = 0;
  *(d1 + 1) = 0;
  *(d1 + 2) = 0;
  if (nclears1 > 3)
    {
      *(d1 + nclears1 - 4) = 0;
      *(d1 + nclears1 - 4 + 1) = 0;
      *(d1 + nclears1 - 4 + 2) = 0;
      *(d1 + nclears1 - 4 + 3) = 0;
    }

  double *d2 = (double *) mem2;

  *(d2 + 0) = 0;
  *(d2 + 1) = 0;
  *(d2 + 2) = 0;
  if (nclears2 > 3)
    {
      *(d2 + nclears2 - 4) = 0;
      *(d2 + nclears2 - 4 + 1) = 0;
      *(d2 + nclears2 - 4 + 2) = 0;
      *(d2 + nclears2 - 4 + 3) = 0;
    }
}
