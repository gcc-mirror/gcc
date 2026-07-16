/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vint8mf8_t
test0_vilo (vint8mf8_t vs2, vint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint8mf4_t
test1_vilo (vint8mf4_t vs2, vint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint8mf2_t
test2_vilo (vint8mf2_t vs2, vint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint8m1_t
test3_vilo (vint8m1_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint8m2_t
test4_vilo (vint8m2_t vs2, vint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint8m4_t
test5_vilo (vint8m4_t vs2, vint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint8m8_t
test6_vilo (vint8m8_t vs2, vint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint16mf4_t
test7_vilo (vint16mf4_t vs2, vint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint16mf2_t
test8_vilo (vint16mf2_t vs2, vint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint16m1_t
test9_vilo (vint16m1_t vs2, vint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint16m2_t
test10_vilo (vint16m2_t vs2, vint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint16m4_t
test11_vilo (vint16m4_t vs2, vint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint16m8_t
test12_vilo (vint16m8_t vs2, vint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint32mf2_t
test13_vilo (vint32mf2_t vs2, vint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint32m1_t
test14_vilo (vint32m1_t vs2, vint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint32m2_t
test15_vilo (vint32m2_t vs2, vint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint32m4_t
test16_vilo (vint32m4_t vs2, vint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint32m8_t
test17_vilo (vint32m8_t vs2, vint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint64m1_t
test18_vilo (vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint64m2_t
test19_vilo (vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint64m4_t
test20_vilo (vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vint64m8_t
test21_vilo (vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint8mf8_t
test22_vilo (vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint8mf4_t
test23_vilo (vuint8mf4_t vs2, vuint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint8mf2_t
test24_vilo (vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint8m1_t
test25_vilo (vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint8m2_t
test26_vilo (vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint8m4_t
test27_vilo (vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint8m8_t
test28_vilo (vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint16mf4_t
test29_vilo (vuint16mf4_t vs2, vuint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint16mf2_t
test30_vilo (vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint16m1_t
test31_vilo (vuint16m1_t vs2, vuint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint16m2_t
test32_vilo (vuint16m2_t vs2, vuint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint16m4_t
test33_vilo (vuint16m4_t vs2, vuint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint16m8_t
test34_vilo (vuint16m8_t vs2, vuint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint32mf2_t
test35_vilo (vuint32mf2_t vs2, vuint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint32m1_t
test36_vilo (vuint32m1_t vs2, vuint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint32m2_t
test37_vilo (vuint32m2_t vs2, vuint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint32m4_t
test38_vilo (vuint32m4_t vs2, vuint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint32m8_t
test39_vilo (vuint32m8_t vs2, vuint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint64m1_t
test40_vilo (vuint64m1_t vs2, vuint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint64m2_t
test41_vilo (vuint64m2_t vs2, vuint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint64m4_t
test42_vilo (vuint64m4_t vs2, vuint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
}

vuint64m8_t
test43_vilo (vuint64m8_t vs2, vuint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo (vs2, vs1, vl);
};

vint8mf8_t
test45_vilo_tu (vint8mf8_t vd, vint8mf8_t vs2, vint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint8mf4_t
test46_vilo_tu (vint8mf4_t vd, vint8mf4_t vs2, vint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint8mf2_t
test47_vilo_tu (vint8mf2_t vd, vint8mf2_t vs2, vint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint8m1_t
test48_vilo_tu (vint8m1_t vd, vint8m1_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint8m2_t
test49_vilo_tu (vint8m2_t vd, vint8m2_t vs2, vint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint8m4_t
test50_vilo_tu (vint8m4_t vd, vint8m4_t vs2, vint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint8m8_t
test51_vilo_tu (vint8m8_t vd, vint8m8_t vs2, vint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint16mf4_t
test52_vilo_tu (vint16mf4_t vd, vint16mf4_t vs2, vint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint16mf2_t
test53_vilo_tu (vint16mf2_t vd, vint16mf2_t vs2, vint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint16m1_t
test54_vilo_tu (vint16m1_t vd, vint16m1_t vs2, vint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint16m2_t
test55_vilo_tu (vint16m2_t vd, vint16m2_t vs2, vint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint16m4_t
test56_vilo_tu (vint16m4_t vd, vint16m4_t vs2, vint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint16m8_t
test57_vilo_tu (vint16m8_t vd, vint16m8_t vs2, vint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint32mf2_t
test58_vilo_tu (vint32mf2_t vd, vint32mf2_t vs2, vint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint32m1_t
test59_vilo_tu (vint32m1_t vd, vint32m1_t vs2, vint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint32m2_t
test60_vilo_tu (vint32m2_t vd, vint32m2_t vs2, vint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint32m4_t
test61_vilo_tu (vint32m4_t vd, vint32m4_t vs2, vint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint32m8_t
test62_vilo_tu (vint32m8_t vd, vint32m8_t vs2, vint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint64m1_t
test63_vilo_tu (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint64m2_t
test64_vilo_tu (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint64m4_t
test65_vilo_tu (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vint64m8_t
test66_vilo_tu (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint8mf8_t
test67_vilo_tu (vuint8mf8_t vd, vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint8mf4_t
test68_vilo_tu (vuint8mf4_t vd, vuint8mf4_t vs2, vuint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint8mf2_t
test69_vilo_tu (vuint8mf2_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint8m1_t
test70_vilo_tu (vuint8m1_t vd, vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint8m2_t
test71_vilo_tu (vuint8m2_t vd, vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint8m4_t
test72_vilo_tu (vuint8m4_t vd, vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint8m8_t
test73_vilo_tu (vuint8m8_t vd, vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint16mf4_t
test74_vilo_tu (vuint16mf4_t vd, vuint16mf4_t vs2, vuint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint16mf2_t
test75_vilo_tu (vuint16mf2_t vd, vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint16m1_t
test76_vilo_tu (vuint16m1_t vd, vuint16m1_t vs2, vuint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint16m2_t
test77_vilo_tu (vuint16m2_t vd, vuint16m2_t vs2, vuint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint16m4_t
test78_vilo_tu (vuint16m4_t vd, vuint16m4_t vs2, vuint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint16m8_t
test79_vilo_tu (vuint16m8_t vd, vuint16m8_t vs2, vuint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint32mf2_t
test80_vilo_tu (vuint32mf2_t vd, vuint32mf2_t vs2, vuint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint32m1_t
test81_vilo_tu (vuint32m1_t vd, vuint32m1_t vs2, vuint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint32m2_t
test82_vilo_tu (vuint32m2_t vd, vuint32m2_t vs2, vuint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint32m4_t
test83_vilo_tu (vuint32m4_t vd, vuint32m4_t vs2, vuint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint32m8_t
test84_vilo_tu (vuint32m8_t vd, vuint32m8_t vs2, vuint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint64m1_t
test85_vilo_tu (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint64m2_t
test86_vilo_tu (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint64m4_t
test87_vilo_tu (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

vuint64m8_t
test88_vilo_tu (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vilo\\.vv\t" 88 } } */
