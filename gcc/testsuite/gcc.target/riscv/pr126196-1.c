/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvksh -mabi=lp64d -O2" { target { rv64 } } } */
/* { dg-options "-march=rv32gcv_zvksh -mabi=ilp32d -O2" { target { rv32 } } } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-O1" "-Os" "-Oz" "-Og" } } */

#include <riscv_vector.h>

vuint32m1_t
f (vuint32m1_t vs2, vuint32m1_t vs1, size_t vl)
{
  return __riscv_vsm3me_vv_u32m1 (vs2,
				  __riscv_vsm3me_vv_u32m1 (vs2, vs1, vl),
				  vl);
}

vuint32m1_t
g (vuint32m1_t a, vuint32m1_t b, vuint32m1_t c, vuint32m1_t d,
   vuint32m1_t e, vuint32m1_t h, vuint32m1_t i, vuint32m1_t j,
   size_t vl)
{
  vuint32m1_t r1 = __riscv_vsm3me_vv_u32m1 (a, b, vl);
  vuint32m1_t r2 = __riscv_vsm3me_vv_u32m1 (c, d, vl);
  vuint32m1_t r3 = __riscv_vsm3me_vv_u32m1 (e, h, vl);
  vuint32m1_t r4 = __riscv_vsm3me_vv_u32m1 (i, j, vl);
  return __riscv_vxor_vv_u32m1 (__riscv_vxor_vv_u32m1 (r1, r2, vl),
				 __riscv_vxor_vv_u32m1 (r3, r4, vl), vl);
}

/* { dg-final { scan-assembler-not {vsm3me\.vv\tv([0-9]+),v\1,} } } */
