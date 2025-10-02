/* { dg-do compile } */
/* { dg-options "-march=rv32gv_xandesvdot -O3 -mabi=ilp32" { target { rv32 } } } */
/* { dg-options "-march=rv64gv_xandesvdot -O3 -mabi=lp64d" { target { rv64 } } } */

#include "andes_vector.h"

vint32mf2_t
test_nds_vd4dots_vv_i32mf2 (vint32mf2_t vd, vint8mf2_t vs1, vint8mf2_t vs2,
			    size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32mf2(vd, vs1, vs2, vl);
}

vint32m1_t
test_nds_vd4dots_vv_i32m1 (vint32m1_t vd, vint8m1_t vs1, vint8m1_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m1(vd, vs1, vs2, vl);
}

vint32m2_t
test_nds_vd4dots_vv_i32m2 (vint32m2_t vd, vint8m2_t vs1, vint8m2_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m2(vd, vs1, vs2, vl);
}

vint32m4_t
test_nds_vd4dots_vv_i32m4 (vint32m4_t vd, vint8m4_t vs1, vint8m4_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m4(vd, vs1, vs2, vl);
}

vint32m8_t
test_nds_vd4dots_vv_i32m8 (vint32m8_t vd, vint8m8_t vs1, vint8m8_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m8(vd, vs1, vs2, vl);
}

vint64m1_t
test_nds_vd4dots_vv_i64m1 (vint64m1_t vd, vint16m1_t vs1, vint16m1_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m1(vd, vs1, vs2, vl);
}

vint64m2_t
test_nds_vd4dots_vv_i64m2 (vint64m2_t vd, vint16m2_t vs1, vint16m2_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m2(vd, vs1, vs2, vl);
}

vint64m4_t
test_nds_vd4dots_vv_i64m4 (vint64m4_t vd, vint16m4_t vs1, vint16m4_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m4(vd, vs1, vs2, vl);
}

vint64m8_t
test_nds_vd4dots_vv_i64m8 (vint64m8_t vd, vint16m8_t vs1, vint16m8_t vs2,
			   size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m8(vd, vs1, vs2, vl);
}

vint32mf2_t
test_nds_vd4dots_vv_i32mf2_m (vbool64_t vm, vint32mf2_t vd, vint8mf2_t vs1,
			      vint8mf2_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32mf2_m (vm, vd, vs1, vs2, vl);
}

vint32m1_t
test_nds_vd4dots_vv_i32m1_m (vbool32_t vm, vint32m1_t vd, vint8m1_t vs1,
			     vint8m1_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m1_m (vm, vd, vs1, vs2, vl);
}

vint32m2_t
test_nds_vd4dots_vv_i32m2_m (vbool16_t vm, vint32m2_t vd, vint8m2_t vs1,
			     vint8m2_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m2_m (vm, vd, vs1, vs2, vl);
}

vint32m4_t
test_nds_vd4dots_vv_i32m4_m (vbool8_t vm, vint32m4_t vd, vint8m4_t vs1,
			     vint8m4_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m4_m (vm, vd, vs1, vs2, vl);
}

vint32m8_t
test_nds_vd4dots_vv_i32m8_m (vbool4_t vm, vint32m8_t vd, vint8m8_t vs1,
			     vint8m8_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i32m8_m (vm, vd, vs1, vs2, vl);
}

vint64m1_t
test_nds_vd4dots_vv_i64m1_m (vbool64_t vm, vint64m1_t vd, vint16m1_t vs1,
			     vint16m1_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m1_m (vm, vd, vs1, vs2, vl);
}

vint64m2_t
test_nds_vd4dots_vv_i64m2_m (vbool32_t vm, vint64m2_t vd, vint16m2_t vs1,
			     vint16m2_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m2_m (vm, vd, vs1, vs2, vl);
}

vint64m4_t
test_nds_vd4dots_vv_i64m4_m (vbool16_t vm, vint64m4_t vd, vint16m4_t vs1,
			     vint16m4_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m4_m (vm, vd, vs1, vs2, vl);
}

vint64m8_t
test_nds_vd4dots_vv_i64m8_m (vbool8_t vm, vint64m8_t vd, vint16m8_t vs1,
			     vint16m8_t vs2, size_t vl)
{
  return __riscv_nds_vd4dots_vv_i64m8_m (vm, vd, vs1, vs2, vl);
}
/* { dg-final { scan-assembler-times {vseti?vli\s+[a-z0-9]+,\s*[a-z0-9]+,\s*e[0-9]+,\s*mf?[1248],\s*t[au],\s*m[au]\s+nds\.vd4dots[ivxfswum.]*\s+} 18 } } */
