/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vint64m1_t
test_vcrcfoldr_vv_i64m1 (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m1 (vd, vs2, vs1, vl);
}

vint64m2_t
test_vcrcfoldr_vv_i64m2 (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m2 (vd, vs2, vs1, vl);
}

vint64m4_t
test_vcrcfoldr_vv_i64m4 (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m4 (vd, vs2, vs1, vl);
}

vint64m8_t
test_vcrcfoldr_vv_i64m8 (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m8 (vd, vs2, vs1, vl);
}

vuint64m1_t
test_vcrcfoldr_vv_u64m1 (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m1 (vd, vs2, vs1, vl);
}

vuint64m2_t
test_vcrcfoldr_vv_u64m2 (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m2 (vd, vs2, vs1, vl);
}

vuint64m4_t
test_vcrcfoldr_vv_u64m4 (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m4 (vd, vs2, vs1, vl);
}

vuint64m8_t
test_vcrcfoldr_vv_u64m8 (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1,
			 size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m8 (vd, vs2, vs1, vl);
}

vint64m1_t
test_vcrcfoldr_vv_i64m1_tu (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m1_tu (vd, vs2, vs1, vl);
}

vint64m2_t
test_vcrcfoldr_vv_i64m2_tu (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m2_tu (vd, vs2, vs1, vl);
}

vint64m4_t
test_vcrcfoldr_vv_i64m4_tu (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m4_tu (vd, vs2, vs1, vl);
}

vint64m8_t
test_vcrcfoldr_vv_i64m8_tu (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_i64m8_tu (vd, vs2, vs1, vl);
}

vuint64m1_t
test_vcrcfoldr_vv_u64m1_tu (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m1_tu (vd, vs2, vs1, vl);
}

vuint64m2_t
test_vcrcfoldr_vv_u64m2_tu (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m2_tu (vd, vs2, vs1, vl);
}

vuint64m4_t
test_vcrcfoldr_vv_u64m4_tu (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m4_tu (vd, vs2, vs1, vl);
}

vuint64m8_t
test_vcrcfoldr_vv_u64m8_tu (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1,
			    size_t vl)
{
  return __riscv_xt_vcrcfoldr_vv_u64m8_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vcrcfoldr\\.vv\t" 16 } } */
