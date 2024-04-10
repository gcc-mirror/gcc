/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

size_t __attribute__ ((noinline))
normalize_vl (size_t vl)
{
  if (vl % 4 == 0)
    return vl;

  return ((vl / 4) + 1) * 4;
}

vfloat32m1_t
test_float_point_dynamic_frm (vfloat32m1_t op1, vfloat32m1_t op2, size_t vl)
{
  vfloat32m1_t result = op1;

  if (vl % 4 != 0)
    vl = normalize_vl (vl);

  return vl > 16 ? result : op2;
}

/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[01234]} } } */
/* { dg-final { scan-assembler-not {fsrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {frrm\s+[axs][0-9]+} } } */
