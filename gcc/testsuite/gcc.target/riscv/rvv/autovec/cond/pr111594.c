/* { dg-do compile } */
/* { dg-additional-options "-march=rv64gcv -mabi=lp64d -fno-vect-cost-model -ffast-math" } */

#include <stdint-gcc.h>

void
pr11594 (uint64_t *restrict a, uint64_t *restrict b, int loop_size)
{
  uint64_t result = 0;

  for (int i = 0; i < loop_size; i++)
    {
      if (b[i] <= a[i])
	{
	  result += a[i];
	}
    }

  a[0] = result;
}

/* { dg-final { scan-assembler-not {vmerge} } } */
