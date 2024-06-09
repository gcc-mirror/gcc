/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ffast-math" } */

#include <stdint-gcc.h>

int64_t
reduc_plus_int (int *__restrict a, int n)
{
  int64_t r = 0;
  for (int i = 0; i < n; ++i)
    r += a[i];
  return r;
}

double
reduc_plus_float (float *__restrict a, int n)
{
  double r = 0;
  for (int i = 0; i < n; ++i)
    r += a[i];
  return r;
}

/* { dg-final { scan-assembler-not {vmv1r} { xfail riscv*-*-* } } } */
/* { dg-final { scan-assembler-not {vmv2r} } } */
/* { dg-final { scan-assembler-not {vmv4r} } } */
/* { dg-final { scan-assembler-not {vmv8r} } } */
/* { dg-final { scan-assembler-not {csrr} } } */
/* { dg-final { scan-assembler-times {vwadd\.wv} 1 } } */
/* { dg-final { scan-assembler-times {vfwadd\.wv} 1 } } */
