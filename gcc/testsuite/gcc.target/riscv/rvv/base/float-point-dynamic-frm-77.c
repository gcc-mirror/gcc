/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zbb -mrvv-vector-bits=zvl -ffast-math -mabi=lp64 -O3" } */

#include "riscv_vector.h"

double sum(double *d)
{
  double sum = 0.0;

  for (int i = 0; i < 8; ++i)
    sum += d[i];

  return sum;
}

/* { dg-final { scan-assembler-times {vfadd\.v[vf]\s+v[0-9]+,\s*v[0-9]+,\s*[fav]+[0-9]+} 3 } } */
/* { dg-final { scan-assembler-not {frrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {fsrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[01234]} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
