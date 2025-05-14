/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */
/* { dg-final { scan-assembler-times "xor\[a-z\]*\[\t \]*%xmm\[0-9\]\+,\[^,\]*" 1 } } */

#include <stddef.h>

float
clear_memory (void *mem, size_t clearsize)
{
  size_t *d = (size_t *) mem;
  size_t nclears = clearsize / sizeof (size_t);

  *(d + 0) = 0;
  *(d + 1) = 0;
  *(d + 2) = 0;
  if (nclears > 9)
    {
      *(d + 5) = 0;
      *(d + 5 + 1) = 0;
      *(d + 5 + 2) = 0;
      *(d + 5 + 3) = 0;
      *(d + nclears - 8) = 0;
      *(d + nclears - 8 + 1) = 0;
      *(d + nclears - 8 + 2) = 0;
      *(d + nclears - 8 + 3) = 0;
    }
  else
    {
      *(d + 1) = 0;
      *(d + 2) = 0;
      *(d + 3) = 0;
      *(d + 4) = 0;
      *(d + nclears - 4) = 0;
      *(d + nclears - 4 + 1) = 0;
      *(d + nclears - 4 + 2) = 0;
      *(d + nclears - 4 + 3) = 0;
    }

  return nclears;
}
