/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vint64m1_t
test0_vcrcfoldr (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
}

vint64m2_t
test1_vcrcfoldr (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
}

vint64m4_t
test2_vcrcfoldr (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
}

vint64m8_t
test3_vcrcfoldr (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
}

vuint64m1_t
test4_vcrcfoldr (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
}

vuint64m2_t
test5_vcrcfoldr (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
}

vuint64m4_t
test6_vcrcfoldr (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
}

vuint64m8_t
test7_vcrcfoldr (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr (vd, vs2, vs1, vl);
};

vint64m1_t
test9_vcrcfoldr_tu (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

vint64m2_t
test10_vcrcfoldr_tu (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

vint64m4_t
test11_vcrcfoldr_tu (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

vint64m8_t
test12_vcrcfoldr_tu (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

vuint64m1_t
test13_vcrcfoldr_tu (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

vuint64m2_t
test14_vcrcfoldr_tu (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

vuint64m4_t
test15_vcrcfoldr_tu (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

vuint64m8_t
test16_vcrcfoldr_tu (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldr_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vcrcfoldr\\.vv\t" 16 } } */