/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vuint8mf8_t
test0_vgmulxor (vuint8mf8_t vd, vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor (vd, vs2, vs1, vl);
}

vuint8mf4_t
test1_vgmulxor (vuint8mf4_t vd, vuint8mf4_t vs2, vuint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor (vd, vs2, vs1, vl);
}

vuint8mf2_t
test2_vgmulxor (vuint8mf2_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor (vd, vs2, vs1, vl);
}

vuint8m1_t
test3_vgmulxor (vuint8m1_t vd, vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor (vd, vs2, vs1, vl);
}

vuint8m2_t
test4_vgmulxor (vuint8m2_t vd, vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor (vd, vs2, vs1, vl);
}

vuint8m4_t
test5_vgmulxor (vuint8m4_t vd, vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor (vd, vs2, vs1, vl);
}

vuint8m8_t
test6_vgmulxor (vuint8m8_t vd, vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor (vd, vs2, vs1, vl);
}

vuint8mf8_t
test8_vgmulxor_tu (vuint8mf8_t vd, vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_tu (vd, vs2, vs1, vl);
}

vuint8mf4_t
test9_vgmulxor_tu (vuint8mf4_t vd, vuint8mf4_t vs2, vuint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_tu (vd, vs2, vs1, vl);
}

vuint8mf2_t
test10_vgmulxor_tu (vuint8mf2_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_tu (vd, vs2, vs1, vl);
}

vuint8m1_t
test11_vgmulxor_tu (vuint8m1_t vd, vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_tu (vd, vs2, vs1, vl);
}

vuint8m2_t
test12_vgmulxor_tu (vuint8m2_t vd, vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_tu (vd, vs2, vs1, vl);
}

vuint8m4_t
test13_vgmulxor_tu (vuint8m4_t vd, vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_tu (vd, vs2, vs1, vl);
}

vuint8m8_t
test14_vgmulxor_tu (vuint8m8_t vd, vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vgmulxor_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vgmulxor\\.vv\t" 14 } } */