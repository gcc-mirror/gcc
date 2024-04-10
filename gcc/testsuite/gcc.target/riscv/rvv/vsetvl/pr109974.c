/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv32gcv_zbb -mabi=ilp32d -mrvv-vector-bits=zvl -O3" } */

#include <stdint-gcc.h>

void
func (int8_t *__restrict x, int64_t *__restrict y, int n)
{
  for (int i = 0, j = 0; i < n; i++, j +=2 )
  {
    x[i + 0] += 1;
    y[j + 0] += 1;
    y[j + 1] += 2;
  }
}

/* { dg-final { scan-assembler {vsetvli} { target { no-opts "-O0" no-opts "-O1" no-opts "-Os" no-opts "-Oz" no-opts "-g" no-opts "-funroll-loops" } } } } */
