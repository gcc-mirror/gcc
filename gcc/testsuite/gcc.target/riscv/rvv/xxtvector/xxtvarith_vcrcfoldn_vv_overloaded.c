/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vint64m1_t
test0_vcrcfoldn (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
}

vint64m2_t
test1_vcrcfoldn (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
}

vint64m4_t
test2_vcrcfoldn (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
}

vint64m8_t
test3_vcrcfoldn (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
}

vuint64m1_t
test4_vcrcfoldn (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
}

vuint64m2_t
test5_vcrcfoldn (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
}

vuint64m4_t
test6_vcrcfoldn (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
}

vuint64m8_t
test7_vcrcfoldn (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn (vd, vs2, vs1, vl);
};

vint64m1_t
test9_vcrcfoldn_tu (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

vint64m2_t
test10_vcrcfoldn_tu (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

vint64m4_t
test11_vcrcfoldn_tu (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

vint64m8_t
test12_vcrcfoldn_tu (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

vuint64m1_t
test13_vcrcfoldn_tu (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

vuint64m2_t
test14_vcrcfoldn_tu (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

vuint64m4_t
test15_vcrcfoldn_tu (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

vuint64m8_t
test16_vcrcfoldn_tu (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1,
		     size_t vl)
{
  return __riscv_xt_vcrcfoldn_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vcrcfoldn\\.vv\t" 16 } } */
