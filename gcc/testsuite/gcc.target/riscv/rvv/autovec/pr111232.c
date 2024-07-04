/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -mrvv-vector-bits=scalable -Ofast -fno-schedule-insns -fno-schedule-insns2" } */

#include <stdint-gcc.h>

int16_t
foo (int8_t *restrict x, int8_t *restrict y, int n)
{
  int16_t result = 0;

  for (int i = 0; i < n; i++)
    {
      result += (x[i] * y[i]);
    }
  return result;
}

/* { dg-final { scan-assembler {\tvwmacc\.vv\tv[0-9]+,v[0-9]+,v[0-9]+} } }  */
