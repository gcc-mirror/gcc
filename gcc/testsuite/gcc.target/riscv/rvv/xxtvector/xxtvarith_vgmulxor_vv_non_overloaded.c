/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vuint8mf8_t
test_vgmulxor_vv_u8mf8 (vuint8mf8_t vd, vuint8mf8_t vs2, vuint8mf8_t vs1,
			size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8mf8 (vd, vs2, vs1, vl);
}

vuint8mf4_t
test_vgmulxor_vv_u8mf4 (vuint8mf4_t vd, vuint8mf4_t vs2, vuint8mf4_t vs1,
			size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8mf4 (vd, vs2, vs1, vl);
}

vuint8mf2_t
test_vgmulxor_vv_u8mf2 (vuint8mf2_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1,
			size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8mf2 (vd, vs2, vs1, vl);
}

vuint8m1_t
test_vgmulxor_vv_u8m1 (vuint8m1_t vd, vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m1 (vd, vs2, vs1, vl);
}

vuint8m2_t
test_vgmulxor_vv_u8m2 (vuint8m2_t vd, vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m2 (vd, vs2, vs1, vl);
}

vuint8m4_t
test_vgmulxor_vv_u8m4 (vuint8m4_t vd, vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m4 (vd, vs2, vs1, vl);
}

vuint8m8_t
test_vgmulxor_vv_u8m8 (vuint8m8_t vd, vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m8 (vd, vs2, vs1, vl);
}

vuint8mf8_t
test_vgmulxor_vv_u8mf8_tu (vuint8mf8_t vd, vuint8mf8_t vs2, vuint8mf8_t vs1,
			   size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8mf8_tu (vd, vs2, vs1, vl);
}

vuint8mf4_t
test_vgmulxor_vv_u8mf4_tu (vuint8mf4_t vd, vuint8mf4_t vs2, vuint8mf4_t vs1,
			   size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8mf4_tu (vd, vs2, vs1, vl);
}

vuint8mf2_t
test_vgmulxor_vv_u8mf2_tu (vuint8mf2_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1,
			   size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8mf2_tu (vd, vs2, vs1, vl);
}

vuint8m1_t
test_vgmulxor_vv_u8m1_tu (vuint8m1_t vd, vuint8m1_t vs2, vuint8m1_t vs1,
			  size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m1_tu (vd, vs2, vs1, vl);
}

vuint8m2_t
test_vgmulxor_vv_u8m2_tu (vuint8m2_t vd, vuint8m2_t vs2, vuint8m2_t vs1,
			  size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m2_tu (vd, vs2, vs1, vl);
}

vuint8m4_t
test_vgmulxor_vv_u8m4_tu (vuint8m4_t vd, vuint8m4_t vs2, vuint8m4_t vs1,
			  size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m4_tu (vd, vs2, vs1, vl);
}

vuint8m8_t
test_vgmulxor_vv_u8m8_tu (vuint8m8_t vd, vuint8m8_t vs2, vuint8m8_t vs1,
			  size_t vl)
{
  return __riscv_xt_vgmulxor_vv_u8m8_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vgmulxor\\.vv\t" 14 } } */