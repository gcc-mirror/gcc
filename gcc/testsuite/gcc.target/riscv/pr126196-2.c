/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvkned_zvksh -mabi=lp64d -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gcv_zvkned_zvksh -mabi=ilp32d -O2" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

#include <riscv_vector.h>

vuint32m1_t
f (vuint32m1_t a, size_t vl)
{
  return __riscv_vsm3c_vi_u32m1 (a, a, 2, vl);
}

vuint32m1_t
g (vuint32m1_t a, size_t vl)
{
  return __riscv_vaeskf2_vi_u32m1 (a, a, 3, vl);
}

/* { dg-final { scan-assembler-not {vsm3c\.vi\tv([0-9]+),v\1,} } } */
/* { dg-final { scan-assembler {vaeskf2\.vi\tv([0-9]+),v\1,} } } */
