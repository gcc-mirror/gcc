/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2 -fno-schedule-insns -fdump-rtl-vsetvl-details" } */

#include "riscv_vector.h"

vuint16m1_t
foo (vuint16m1_t a, vuint16m1_t b, size_t avl)
{
  size_t vl;
  vuint16m1_t ret;
  uint16_t c = __riscv_vmv_x_s_u16m1_u16(a);
  vl = __riscv_vsetvl_e8mf2 (avl);
  ret = __riscv_vadd_vx_u16m1 (a, c, avl);
  ret = __riscv_vadd_vv_u16m1 (ret, a, vl);
  return ret;
}

/* { dg-final { scan-rtl-dump "Eliminate insn" "vsetvl" } }  */
/* { dg-final { scan-assembler-times {vsetvli} 2 } } */
