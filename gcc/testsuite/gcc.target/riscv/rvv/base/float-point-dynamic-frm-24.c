/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */

#include "riscv_vector.h"

void
test_float_point_dynamic_frm (vfloat32m1_t *out, vfloat32m1_t op1, size_t vl)
{
}

/* { dg-final { scan-assembler-not {fsrmi\s+[axs][0-9]+,\s*[01234]} } } */
/* { dg-final { scan-assembler-not {fsrmi\s+[01234]} } } */
/* { dg-final { scan-assembler-not {fsrm\s+[axs][0-9]+} } } */
/* { dg-final { scan-assembler-not {frrm\s+[axs][0-9]+} } } */
