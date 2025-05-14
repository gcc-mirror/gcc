/* { dg-do assemble } */
/* { dg-options "-march=rv32gc_xtheadvector -mabi=ilp32d -O2 -save-temps" { target { rv32 } } } */
/* { dg-options "-march=rv64gc_xtheadvector -mabi=lp64d -O2 -save-temps" { target { rv64 } } } */

#include "riscv_vector.h"

void
foo (float *a, int b)
{
  vfloat32m1x4_t c;
  __riscv_vsseg4e32_v_f32m1x4(a, c, b);
}

/* { dg-final { scan-assembler-times {th\.vmv\.v\.i\tv[0-9]+,0} 4 } } */
