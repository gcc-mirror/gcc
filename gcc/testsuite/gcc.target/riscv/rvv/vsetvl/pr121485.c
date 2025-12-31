/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvkned -mabi=lp64d" { target rv64 } } */
/* { dg-options "-march=rv32gcv_zvkned -mabi=ilp32" { target rv32 } } */

#include <riscv_vector.h>

vuint32m4_t test_riscv_vaesz_vs_u32m1_u32m4(vuint32m4_t a, vuint32m1_t b, int vl)
{
  return __riscv_vaesz_vs_u32m1_u32m4(a, b, vl);
}


/* { dg-final { scan-assembler {vsetvli\szero,[a-x0-9]+,e32,m4,ta,ma} } } */
