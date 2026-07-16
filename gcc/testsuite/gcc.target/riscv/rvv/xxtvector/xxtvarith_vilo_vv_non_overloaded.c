/* { dg-do compile } */
/* { dg-options "-march=rv32gcv_xxtvarith -mabi=ilp32d" { target { rv32 } } } */
/* { dg-options "-march=rv64gcv_xxtvarith -mabi=lp64d" { target { rv64 } } } */

#include "xuantie_vector.h"

vint8mf8_t
test_vilo_vv_i8mf8 (vint8mf8_t vs2, vint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8mf8 (vs2, vs1, vl);
}

vint8mf4_t
test_vilo_vv_i8mf4 (vint8mf4_t vs2, vint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8mf4 (vs2, vs1, vl);
}

vint8mf2_t
test_vilo_vv_i8mf2 (vint8mf2_t vs2, vint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8mf2 (vs2, vs1, vl);
}

vint8m1_t
test_vilo_vv_i8m1 (vint8m1_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m1 (vs2, vs1, vl);
}

vint8m2_t
test_vilo_vv_i8m2 (vint8m2_t vs2, vint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m2 (vs2, vs1, vl);
}

vint8m4_t
test_vilo_vv_i8m4 (vint8m4_t vs2, vint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m4 (vs2, vs1, vl);
}

vint8m8_t
test_vilo_vv_i8m8 (vint8m8_t vs2, vint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m8 (vs2, vs1, vl);
}

vint16mf4_t
test_vilo_vv_i16mf4 (vint16mf4_t vs2, vint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16mf4 (vs2, vs1, vl);
}

vint16mf2_t
test_vilo_vv_i16mf2 (vint16mf2_t vs2, vint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16mf2 (vs2, vs1, vl);
}

vint16m1_t
test_vilo_vv_i16m1 (vint16m1_t vs2, vint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m1 (vs2, vs1, vl);
}

vint16m2_t
test_vilo_vv_i16m2 (vint16m2_t vs2, vint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m2 (vs2, vs1, vl);
}

vint16m4_t
test_vilo_vv_i16m4 (vint16m4_t vs2, vint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m4 (vs2, vs1, vl);
}

vint16m8_t
test_vilo_vv_i16m8 (vint16m8_t vs2, vint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m8 (vs2, vs1, vl);
}

vint32mf2_t
test_vilo_vv_i32mf2 (vint32mf2_t vs2, vint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32mf2 (vs2, vs1, vl);
}

vint32m1_t
test_vilo_vv_i32m1 (vint32m1_t vs2, vint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m1 (vs2, vs1, vl);
}

vint32m2_t
test_vilo_vv_i32m2 (vint32m2_t vs2, vint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m2 (vs2, vs1, vl);
}

vint32m4_t
test_vilo_vv_i32m4 (vint32m4_t vs2, vint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m4 (vs2, vs1, vl);
}

vint32m8_t
test_vilo_vv_i32m8 (vint32m8_t vs2, vint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m8 (vs2, vs1, vl);
}

vint64m1_t
test_vilo_vv_i64m1 (vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m1 (vs2, vs1, vl);
}

vint64m2_t
test_vilo_vv_i64m2 (vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m2 (vs2, vs1, vl);
}

vint64m4_t
test_vilo_vv_i64m4 (vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m4 (vs2, vs1, vl);
}

vint64m8_t
test_vilo_vv_i64m8 (vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m8 (vs2, vs1, vl);
}

vuint8mf8_t
test_vilo_vv_u8mf8 (vuint8mf8_t vs2, vuint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8mf8 (vs2, vs1, vl);
}

vuint8mf4_t
test_vilo_vv_u8mf4 (vuint8mf4_t vs2, vuint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8mf4 (vs2, vs1, vl);
}

vuint8mf2_t
test_vilo_vv_u8mf2 (vuint8mf2_t vs2, vuint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8mf2 (vs2, vs1, vl);
}

vuint8m1_t
test_vilo_vv_u8m1 (vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m1 (vs2, vs1, vl);
}

vuint8m2_t
test_vilo_vv_u8m2 (vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m2 (vs2, vs1, vl);
}

vuint8m4_t
test_vilo_vv_u8m4 (vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m4 (vs2, vs1, vl);
}

vuint8m8_t
test_vilo_vv_u8m8 (vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m8 (vs2, vs1, vl);
}

vuint16mf4_t
test_vilo_vv_u16mf4 (vuint16mf4_t vs2, vuint16mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u16mf4 (vs2, vs1, vl);
}

vuint16mf2_t
test_vilo_vv_u16mf2 (vuint16mf2_t vs2, vuint16mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u16mf2 (vs2, vs1, vl);
}

vuint16m1_t
test_vilo_vv_u16m1 (vuint16m1_t vs2, vuint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u16m1 (vs2, vs1, vl);
}

vuint16m2_t
test_vilo_vv_u16m2 (vuint16m2_t vs2, vuint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u16m2 (vs2, vs1, vl);
}

vuint16m4_t
test_vilo_vv_u16m4 (vuint16m4_t vs2, vuint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u16m4 (vs2, vs1, vl);
}

vuint16m8_t
test_vilo_vv_u16m8 (vuint16m8_t vs2, vuint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u16m8 (vs2, vs1, vl);
}

vuint32mf2_t
test_vilo_vv_u32mf2 (vuint32mf2_t vs2, vuint32mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u32mf2 (vs2, vs1, vl);
}

vuint32m1_t
test_vilo_vv_u32m1 (vuint32m1_t vs2, vuint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u32m1 (vs2, vs1, vl);
}

vuint32m2_t
test_vilo_vv_u32m2 (vuint32m2_t vs2, vuint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u32m2 (vs2, vs1, vl);
}

vuint32m4_t
test_vilo_vv_u32m4 (vuint32m4_t vs2, vuint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u32m4 (vs2, vs1, vl);
}

vuint32m8_t
test_vilo_vv_u32m8 (vuint32m8_t vs2, vuint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u32m8 (vs2, vs1, vl);
}

vuint64m1_t
test_vilo_vv_u64m1 (vuint64m1_t vs2, vuint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u64m1 (vs2, vs1, vl);
}

vuint64m2_t
test_vilo_vv_u64m2 (vuint64m2_t vs2, vuint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u64m2 (vs2, vs1, vl);
}

vuint64m4_t
test_vilo_vv_u64m4 (vuint64m4_t vs2, vuint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u64m4 (vs2, vs1, vl);
}

vuint64m8_t
test_vilo_vv_u64m8 (vuint64m8_t vs2, vuint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u64m8 (vs2, vs1, vl);
}

vint8mf8_t
test_vilo_vv_i8mf8_tu (vint8mf8_t vd, vint8mf8_t vs2, vint8mf8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8mf8_tu (vd, vs2, vs1, vl);
}

vint8mf4_t
test_vilo_vv_i8mf4_tu (vint8mf4_t vd, vint8mf4_t vs2, vint8mf4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8mf4_tu (vd, vs2, vs1, vl);
}

vint8mf2_t
test_vilo_vv_i8mf2_tu (vint8mf2_t vd, vint8mf2_t vs2, vint8mf2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8mf2_tu (vd, vs2, vs1, vl);
}

vint8m1_t
test_vilo_vv_i8m1_tu (vint8m1_t vd, vint8m1_t vs2, vint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m1_tu (vd, vs2, vs1, vl);
}

vint8m2_t
test_vilo_vv_i8m2_tu (vint8m2_t vd, vint8m2_t vs2, vint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m2_tu (vd, vs2, vs1, vl);
}

vint8m4_t
test_vilo_vv_i8m4_tu (vint8m4_t vd, vint8m4_t vs2, vint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m4_tu (vd, vs2, vs1, vl);
}

vint8m8_t
test_vilo_vv_i8m8_tu (vint8m8_t vd, vint8m8_t vs2, vint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i8m8_tu (vd, vs2, vs1, vl);
}

vint16mf4_t
test_vilo_vv_i16mf4_tu (vint16mf4_t vd, vint16mf4_t vs2, vint16mf4_t vs1,
			size_t vl)
{
  return __riscv_xt_vilo_vv_i16mf4_tu (vd, vs2, vs1, vl);
}

vint16mf2_t
test_vilo_vv_i16mf2_tu (vint16mf2_t vd, vint16mf2_t vs2, vint16mf2_t vs1,
			size_t vl)
{
  return __riscv_xt_vilo_vv_i16mf2_tu (vd, vs2, vs1, vl);
}

vint16m1_t
test_vilo_vv_i16m1_tu (vint16m1_t vd, vint16m1_t vs2, vint16m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m1_tu (vd, vs2, vs1, vl);
}

vint16m2_t
test_vilo_vv_i16m2_tu (vint16m2_t vd, vint16m2_t vs2, vint16m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m2_tu (vd, vs2, vs1, vl);
}

vint16m4_t
test_vilo_vv_i16m4_tu (vint16m4_t vd, vint16m4_t vs2, vint16m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m4_tu (vd, vs2, vs1, vl);
}

vint16m8_t
test_vilo_vv_i16m8_tu (vint16m8_t vd, vint16m8_t vs2, vint16m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i16m8_tu (vd, vs2, vs1, vl);
}

vint32mf2_t
test_vilo_vv_i32mf2_tu (vint32mf2_t vd, vint32mf2_t vs2, vint32mf2_t vs1,
			size_t vl)
{
  return __riscv_xt_vilo_vv_i32mf2_tu (vd, vs2, vs1, vl);
}

vint32m1_t
test_vilo_vv_i32m1_tu (vint32m1_t vd, vint32m1_t vs2, vint32m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m1_tu (vd, vs2, vs1, vl);
}

vint32m2_t
test_vilo_vv_i32m2_tu (vint32m2_t vd, vint32m2_t vs2, vint32m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m2_tu (vd, vs2, vs1, vl);
}

vint32m4_t
test_vilo_vv_i32m4_tu (vint32m4_t vd, vint32m4_t vs2, vint32m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m4_tu (vd, vs2, vs1, vl);
}

vint32m8_t
test_vilo_vv_i32m8_tu (vint32m8_t vd, vint32m8_t vs2, vint32m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i32m8_tu (vd, vs2, vs1, vl);
}

vint64m1_t
test_vilo_vv_i64m1_tu (vint64m1_t vd, vint64m1_t vs2, vint64m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m1_tu (vd, vs2, vs1, vl);
}

vint64m2_t
test_vilo_vv_i64m2_tu (vint64m2_t vd, vint64m2_t vs2, vint64m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m2_tu (vd, vs2, vs1, vl);
}

vint64m4_t
test_vilo_vv_i64m4_tu (vint64m4_t vd, vint64m4_t vs2, vint64m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m4_tu (vd, vs2, vs1, vl);
}

vint64m8_t
test_vilo_vv_i64m8_tu (vint64m8_t vd, vint64m8_t vs2, vint64m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_i64m8_tu (vd, vs2, vs1, vl);
}

vuint8mf8_t
test_vilo_vv_u8mf8_tu (vuint8mf8_t vd, vuint8mf8_t vs2, vuint8mf8_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u8mf8_tu (vd, vs2, vs1, vl);
}

vuint8mf4_t
test_vilo_vv_u8mf4_tu (vuint8mf4_t vd, vuint8mf4_t vs2, vuint8mf4_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u8mf4_tu (vd, vs2, vs1, vl);
}

vuint8mf2_t
test_vilo_vv_u8mf2_tu (vuint8mf2_t vd, vuint8mf2_t vs2, vuint8mf2_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u8mf2_tu (vd, vs2, vs1, vl);
}

vuint8m1_t
test_vilo_vv_u8m1_tu (vuint8m1_t vd, vuint8m1_t vs2, vuint8m1_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m1_tu (vd, vs2, vs1, vl);
}

vuint8m2_t
test_vilo_vv_u8m2_tu (vuint8m2_t vd, vuint8m2_t vs2, vuint8m2_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m2_tu (vd, vs2, vs1, vl);
}

vuint8m4_t
test_vilo_vv_u8m4_tu (vuint8m4_t vd, vuint8m4_t vs2, vuint8m4_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m4_tu (vd, vs2, vs1, vl);
}

vuint8m8_t
test_vilo_vv_u8m8_tu (vuint8m8_t vd, vuint8m8_t vs2, vuint8m8_t vs1, size_t vl)
{
  return __riscv_xt_vilo_vv_u8m8_tu (vd, vs2, vs1, vl);
}

vuint16mf4_t
test_vilo_vv_u16mf4_tu (vuint16mf4_t vd, vuint16mf4_t vs2, vuint16mf4_t vs1,
			size_t vl)
{
  return __riscv_xt_vilo_vv_u16mf4_tu (vd, vs2, vs1, vl);
}

vuint16mf2_t
test_vilo_vv_u16mf2_tu (vuint16mf2_t vd, vuint16mf2_t vs2, vuint16mf2_t vs1,
			size_t vl)
{
  return __riscv_xt_vilo_vv_u16mf2_tu (vd, vs2, vs1, vl);
}

vuint16m1_t
test_vilo_vv_u16m1_tu (vuint16m1_t vd, vuint16m1_t vs2, vuint16m1_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u16m1_tu (vd, vs2, vs1, vl);
}

vuint16m2_t
test_vilo_vv_u16m2_tu (vuint16m2_t vd, vuint16m2_t vs2, vuint16m2_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u16m2_tu (vd, vs2, vs1, vl);
}

vuint16m4_t
test_vilo_vv_u16m4_tu (vuint16m4_t vd, vuint16m4_t vs2, vuint16m4_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u16m4_tu (vd, vs2, vs1, vl);
}

vuint16m8_t
test_vilo_vv_u16m8_tu (vuint16m8_t vd, vuint16m8_t vs2, vuint16m8_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u16m8_tu (vd, vs2, vs1, vl);
}

vuint32mf2_t
test_vilo_vv_u32mf2_tu (vuint32mf2_t vd, vuint32mf2_t vs2, vuint32mf2_t vs1,
			size_t vl)
{
  return __riscv_xt_vilo_vv_u32mf2_tu (vd, vs2, vs1, vl);
}

vuint32m1_t
test_vilo_vv_u32m1_tu (vuint32m1_t vd, vuint32m1_t vs2, vuint32m1_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u32m1_tu (vd, vs2, vs1, vl);
}

vuint32m2_t
test_vilo_vv_u32m2_tu (vuint32m2_t vd, vuint32m2_t vs2, vuint32m2_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u32m2_tu (vd, vs2, vs1, vl);
}

vuint32m4_t
test_vilo_vv_u32m4_tu (vuint32m4_t vd, vuint32m4_t vs2, vuint32m4_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u32m4_tu (vd, vs2, vs1, vl);
}

vuint32m8_t
test_vilo_vv_u32m8_tu (vuint32m8_t vd, vuint32m8_t vs2, vuint32m8_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u32m8_tu (vd, vs2, vs1, vl);
}

vuint64m1_t
test_vilo_vv_u64m1_tu (vuint64m1_t vd, vuint64m1_t vs2, vuint64m1_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u64m1_tu (vd, vs2, vs1, vl);
}

vuint64m2_t
test_vilo_vv_u64m2_tu (vuint64m2_t vd, vuint64m2_t vs2, vuint64m2_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u64m2_tu (vd, vs2, vs1, vl);
}

vuint64m4_t
test_vilo_vv_u64m4_tu (vuint64m4_t vd, vuint64m4_t vs2, vuint64m4_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u64m4_tu (vd, vs2, vs1, vl);
}

vuint64m8_t
test_vilo_vv_u64m8_tu (vuint64m8_t vd, vuint64m8_t vs2, vuint64m8_t vs1,
		       size_t vl)
{
  return __riscv_xt_vilo_vv_u64m8_tu (vd, vs2, vs1, vl);
}

/* { dg-final { scan-assembler-times "xt\\.vilo\\.vv\t" 88 } } */
