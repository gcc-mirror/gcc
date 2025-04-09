/* { dg-do compile } */
/* { dg-options "-march=rv32gcv -mabi=ilp32d -O2 -fdump-rtl-vsetvl-details" } */

#include "riscv_vector.h"

uint64_t a[2], b[2];

void
foo ()
{
  size_t vl = __riscv_vsetvl_e64m1 (2);
  vuint64m1_t vx = __riscv_vle64_v_u64m1 (a, vl);
  vx = __riscv_vslide1down_vx_u64m1 (vx, 0xffffffffull, vl);
  __riscv_vse64_v_u64m1 (b, vx, vl);
}

/* { dg-final { scan-rtl-dump-not "Eliminate insn" "vsetvl" } }  */
