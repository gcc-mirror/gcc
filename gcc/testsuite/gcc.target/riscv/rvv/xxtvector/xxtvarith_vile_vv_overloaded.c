/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vint8mf8_t
test0_vile (vint8mf8_t vs2, vint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint8mf4_t
test1_vile (vint8mf4_t vs2, vint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint8mf2_t
test2_vile (vint8mf2_t vs2, vint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint8m1_t
test3_vile (vint8m1_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint8m2_t
test4_vile (vint8m2_t vs2, vint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint8m4_t
test5_vile (vint8m4_t vs2, vint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint8m8_t
test6_vile (vint8m8_t vs2, vint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint16mf4_t
test7_vile (vint16mf4_t vs2, vint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint16mf2_t
test8_vile (vint16mf2_t vs2, vint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint16m1_t
test9_vile (vint16m1_t vs2, vint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint16m2_t
test10_vile (vint16m2_t vs2, vint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint16m4_t
test11_vile (vint16m4_t vs2, vint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint16m8_t
test12_vile (vint16m8_t vs2, vint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint32mf2_t
test13_vile (vint32mf2_t vs2, vint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint32m1_t
test14_vile (vint32m1_t vs2, vint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint32m2_t
test15_vile (vint32m2_t vs2, vint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint32m4_t
test16_vile (vint32m4_t vs2, vint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint32m8_t
test17_vile (vint32m8_t vs2, vint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint64m1_t
test18_vile (vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint64m2_t
test19_vile (vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint64m4_t
test20_vile (vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vint64m8_t
test21_vile (vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint8mf8_t
test22_vile (vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint8mf4_t
test23_vile (vuint8mf4_t vs2, vuint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint8mf2_t
test24_vile (vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint8m1_t
test25_vile (vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint8m2_t
test26_vile (vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint8m4_t
test27_vile (vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint8m8_t
test28_vile (vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint16mf4_t
test29_vile (vuint16mf4_t vs2, vuint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint16mf2_t
test30_vile (vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint16m1_t
test31_vile (vuint16m1_t vs2, vuint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint16m2_t
test32_vile (vuint16m2_t vs2, vuint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint16m4_t
test33_vile (vuint16m4_t vs2, vuint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint16m8_t
test34_vile (vuint16m8_t vs2, vuint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint32mf2_t
test35_vile (vuint32mf2_t vs2, vuint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint32m1_t
test36_vile (vuint32m1_t vs2, vuint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint32m2_t
test37_vile (vuint32m2_t vs2, vuint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint32m4_t
test38_vile (vuint32m4_t vs2, vuint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint32m8_t
test39_vile (vuint32m8_t vs2, vuint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint64m1_t
test40_vile (vuint64m1_t vs2, vuint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint64m2_t
test41_vile (vuint64m2_t vs2, vuint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint64m4_t
test42_vile (vuint64m4_t vs2, vuint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
}

vuint64m8_t
test43_vile (vuint64m8_t vs2, vuint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vile (vs2, vs1, vl);
};

vint8mf8_t
test45_vile_tu (vint8mf8_t vd, vint8mf8_t vs2, vint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint8mf4_t
test46_vile_tu (vint8mf4_t vd, vint8mf4_t vs2, vint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint8mf2_t
test47_vile_tu (vint8mf2_t vd, vint8mf2_t vs2, vint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint8m1_t
test48_vile_tu (vint8m1_t vd, vint8m1_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint8m2_t
test49_vile_tu (vint8m2_t vd, vint8m2_t vs2, vint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint8m4_t
test50_vile_tu (vint8m4_t vd, vint8m4_t vs2, vint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint8m8_t
test51_vile_tu (vint8m8_t vd, vint8m8_t vs2, vint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint16mf4_t
test52_vile_tu (vint16mf4_t vd, vint16mf4_t vs2, vint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint16mf2_t
test53_vile_tu (vint16mf2_t vd, vint16mf2_t vs2, vint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint16m1_t
test54_vile_tu (vint16m1_t vd, vint16m1_t vs2, vint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint16m2_t
test55_vile_tu (vint16m2_t vd, vint16m2_t vs2, vint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint16m4_t
test56_vile_tu (vint16m4_t vd, vint16m4_t vs2, vint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint16m8_t
test57_vile_tu (vint16m8_t vd, vint16m8_t vs2, vint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint32mf2_t
test58_vile_tu (vint32mf2_t vd, vint32mf2_t vs2, vint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint32m1_t
test59_vile_tu (vint32m1_t vd, vint32m1_t vs2, vint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint32m2_t
test60_vile_tu (vint32m2_t vd, vint32m2_t vs2, vint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint32m4_t
test61_vile_tu (vint32m4_t vd, vint32m4_t vs2, vint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint32m8_t
test62_vile_tu (vint32m8_t vd, vint32m8_t vs2, vint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint64m1_t
test63_vile_tu (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint64m2_t
test64_vile_tu (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint64m4_t
test65_vile_tu (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vint64m8_t
test66_vile_tu (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint8mf8_t
test67_vile_tu (vuint8mf8_t vd, vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint8mf4_t
test68_vile_tu (vuint8mf4_t vd, vuint8mf4_t vs2, vuint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint8mf2_t
test69_vile_tu (vuint8mf2_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint8m1_t
test70_vile_tu (vuint8m1_t vd, vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint8m2_t
test71_vile_tu (vuint8m2_t vd, vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint8m4_t
test72_vile_tu (vuint8m4_t vd, vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint8m8_t
test73_vile_tu (vuint8m8_t vd, vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint16mf4_t
test74_vile_tu (vuint16mf4_t vd, vuint16mf4_t vs2, vuint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint16mf2_t
test75_vile_tu (vuint16mf2_t vd, vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint16m1_t
test76_vile_tu (vuint16m1_t vd, vuint16m1_t vs2, vuint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint16m2_t
test77_vile_tu (vuint16m2_t vd, vuint16m2_t vs2, vuint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint16m4_t
test78_vile_tu (vuint16m4_t vd, vuint16m4_t vs2, vuint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint16m8_t
test79_vile_tu (vuint16m8_t vd, vuint16m8_t vs2, vuint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint32mf2_t
test80_vile_tu (vuint32mf2_t vd, vuint32mf2_t vs2, vuint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint32m1_t
test81_vile_tu (vuint32m1_t vd, vuint32m1_t vs2, vuint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint32m2_t
test82_vile_tu (vuint32m2_t vd, vuint32m2_t vs2, vuint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint32m4_t
test83_vile_tu (vuint32m4_t vd, vuint32m4_t vs2, vuint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint32m8_t
test84_vile_tu (vuint32m8_t vd, vuint32m8_t vs2, vuint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint64m1_t
test85_vile_tu (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint64m2_t
test86_vile_tu (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint64m4_t
test87_vile_tu (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

vuint64m8_t
test88_vile_tu (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vile_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vile\\.vv\t" 88 } } */
