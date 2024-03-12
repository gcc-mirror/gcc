/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */
/* { dg-additional-options "-fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

vfloat16m2_t
test_vcreate_v_f16m1_f16m2 (vfloat16m1_t v0, vfloat16m1_t v1)
{
  return __riscv_vcreate_v_f16m1_f16m2 (v0, v1);
}

vfloat32m2_t
test_vcreate_v_f32m1_f32m2 (vfloat32m1_t v0, vfloat32m1_t v1)
{
  return __riscv_vcreate_v_f32m1_f32m2 (v0, v1);
}

vfloat64m4_t
test_vcreate_v_f64m1_f64m4 (vfloat64m1_t v0, vfloat64m1_t v1, vfloat64m1_t v2,
			    vfloat64m1_t v3)
{
  return __riscv_vcreate_v_f64m1_f64m4 (v0, v1, v2, v3);
}

vint8m2_t
test_vcreate_v_i8m1_i8m2 (vint8m1_t v0, vint8m1_t v1)
{
  return __riscv_vcreate_v_i8m1_i8m2 (v0, v1);
}

vint16m8_t
test_vcreate_v_i16m1_i16m8 (vint16m1_t v0, vint16m1_t v1, vint16m1_t v2,
			    vint16m1_t v3, vint16m1_t v4, vint16m1_t v5,
			    vint16m1_t v6, vint16m1_t v7)
{
  return __riscv_vcreate_v_i16m1_i16m8 (v0, v1, v2, v3, v4, v5, v6, v7);
}

vint32m4_t
test_vcreate_v_i32m2_i32m4 (vint32m2_t v0, vint32m2_t v1)
{
  return __riscv_vcreate_v_i32m2_i32m4 (v0, v1);
}

vint64m8_t
test_vcreate_v_i64m2_i64m8 (vint64m2_t v0, vint64m2_t v1, vint64m2_t v2,
			    vint64m2_t v3)
{
  return __riscv_vcreate_v_i64m2_i64m8 (v0, v1, v2, v3);
}

vuint8m2_t
test_vcreate_v_u8m1_u8m2 (vuint8m1_t v0, vuint8m1_t v1)
{
  return __riscv_vcreate_v_u8m1_u8m2 (v0, v1);
}

vuint16m8_t
test_vcreate_v_u16m1_u16m8 (vuint16m1_t v0, vuint16m1_t v1, vuint16m1_t v2,
			    vuint16m1_t v3, vuint16m1_t v4, vuint16m1_t v5,
			    vuint16m1_t v6, vuint16m1_t v7)
{
  return __riscv_vcreate_v_u16m1_u16m8 (v0, v1, v2, v3, v4, v5, v6, v7);
}

vuint32m8_t
test_vcreate_v_u32m2_u32m8 (vuint32m2_t v0, vuint32m2_t v1, vuint32m2_t v2,
			    vuint32m2_t v3)
{
  return __riscv_vcreate_v_u32m2_u32m8 (v0, v1, v2, v3);
}

vuint64m4_t
test_vcreate_v_u64m2_u64m4 (vuint64m2_t v0, vuint64m2_t v1)
{
  return __riscv_vcreate_v_u64m2_u64m4 (v0, v1);
}

vfloat16mf4x2_t
test_vcreate_v_f16mf4x2 (vfloat16mf4_t v0, vfloat16mf4_t v1)
{
  return __riscv_vcreate_v_f16mf4x2 (v0, v1);
}

vfloat16mf4x3_t
test_vcreate_v_f16mf4x3 (vfloat16mf4_t v0, vfloat16mf4_t v1, vfloat16mf4_t v2)
{
  return __riscv_vcreate_v_f16mf4x3 (v0, v1, v2);
}

vfloat16mf4x4_t
test_vcreate_v_f16mf4x4 (vfloat16mf4_t v0, vfloat16mf4_t v1, vfloat16mf4_t v2,
			 vfloat16mf4_t v3)
{
  return __riscv_vcreate_v_f16mf4x4 (v0, v1, v2, v3);
}

vfloat16mf4x5_t
test_vcreate_v_f16mf4x5 (vfloat16mf4_t v0, vfloat16mf4_t v1, vfloat16mf4_t v2,
			 vfloat16mf4_t v3, vfloat16mf4_t v4)
{
  return __riscv_vcreate_v_f16mf4x5 (v0, v1, v2, v3, v4);
}

vfloat16mf4x6_t
test_vcreate_v_f16mf4x6 (vfloat16mf4_t v0, vfloat16mf4_t v1, vfloat16mf4_t v2,
			 vfloat16mf4_t v3, vfloat16mf4_t v4, vfloat16mf4_t v5)
{
  return __riscv_vcreate_v_f16mf4x6 (v0, v1, v2, v3, v4, v5);
}

vfloat16mf4x7_t
test_vcreate_v_f16mf4x7 (vfloat16mf4_t v0, vfloat16mf4_t v1, vfloat16mf4_t v2,
			 vfloat16mf4_t v3, vfloat16mf4_t v4, vfloat16mf4_t v5,
			 vfloat16mf4_t v6)
{
  return __riscv_vcreate_v_f16mf4x7 (v0, v1, v2, v3, v4, v5, v6);
}

vfloat16mf4x8_t
test_vcreate_v_f16mf4x8 (vfloat16mf4_t v0, vfloat16mf4_t v1, vfloat16mf4_t v2,
			 vfloat16mf4_t v3, vfloat16mf4_t v4, vfloat16mf4_t v5,
			 vfloat16mf4_t v6, vfloat16mf4_t v7)
{
  return __riscv_vcreate_v_f16mf4x8 (v0, v1, v2, v3, v4, v5, v6, v7);
}

vfloat32m1x2_t
test_vcreate_v_f32m1x2 (vfloat32m1_t v0, vfloat32m1_t v1)
{
  return __riscv_vcreate_v_f32m1x2 (v0, v1);
}

vfloat32m1x3_t
test_vcreate_v_f32m1x3 (vfloat32m1_t v0, vfloat32m1_t v1, vfloat32m1_t v2)
{
  return __riscv_vcreate_v_f32m1x3 (v0, v1, v2);
}

vfloat32m1x4_t
test_vcreate_v_f32m1x4 (vfloat32m1_t v0, vfloat32m1_t v1, vfloat32m1_t v2,
			vfloat32m1_t v3)
{
  return __riscv_vcreate_v_f32m1x4 (v0, v1, v2, v3);
}

vfloat32m1x5_t
test_vcreate_v_f32m1x5 (vfloat32m1_t v0, vfloat32m1_t v1, vfloat32m1_t v2,
			vfloat32m1_t v3, vfloat32m1_t v4)
{
  return __riscv_vcreate_v_f32m1x5 (v0, v1, v2, v3, v4);
}

vfloat32m1x6_t
test_vcreate_v_f32m1x6 (vfloat32m1_t v0, vfloat32m1_t v1, vfloat32m1_t v2,
			vfloat32m1_t v3, vfloat32m1_t v4, vfloat32m1_t v5)
{
  return __riscv_vcreate_v_f32m1x6 (v0, v1, v2, v3, v4, v5);
}

vfloat32m1x7_t
test_vcreate_v_f32m1x7 (vfloat32m1_t v0, vfloat32m1_t v1, vfloat32m1_t v2,
			vfloat32m1_t v3, vfloat32m1_t v4, vfloat32m1_t v5,
			vfloat32m1_t v6)
{
  return __riscv_vcreate_v_f32m1x7 (v0, v1, v2, v3, v4, v5, v6);
}

vfloat32m1x8_t
test_vcreate_v_f32m1x8 (vfloat32m1_t v0, vfloat32m1_t v1, vfloat32m1_t v2,
			vfloat32m1_t v3, vfloat32m1_t v4, vfloat32m1_t v5,
			vfloat32m1_t v6, vfloat32m1_t v7)
{
  return __riscv_vcreate_v_f32m1x8 (v0, v1, v2, v3, v4, v5, v6, v7);
}

vfloat64m2x2_t
test_vcreate_v_f64m2x2 (vfloat64m2_t v0, vfloat64m2_t v1)
{
  return __riscv_vcreate_v_f64m2x2 (v0, v1);
}

vfloat64m2x3_t
test_vcreate_v_f64m2x3 (vfloat64m2_t v0, vfloat64m2_t v1, vfloat64m2_t v2)
{
  return __riscv_vcreate_v_f64m2x3 (v0, v1, v2);
}

vfloat64m2x4_t
test_vcreate_v_f64m2x4 (vfloat64m2_t v0, vfloat64m2_t v1, vfloat64m2_t v2,
			vfloat64m2_t v3)
{
  return __riscv_vcreate_v_f64m2x4 (v0, v1, v2, v3);
}

vfloat64m4x2_t
test_vcreate_v_f64m4x2 (vfloat64m4_t v0, vfloat64m4_t v1)
{
  return __riscv_vcreate_v_f64m4x2 (v0, v1);
}

vint8m2x2_t
test_vcreate_v_i8m2x2 (vint8m2_t v0, vint8m2_t v1)
{
  return __riscv_vcreate_v_i8m2x2 (v0, v1);
}

vint8m2x3_t
test_vcreate_v_i8m2x3 (vint8m2_t v0, vint8m2_t v1, vint8m2_t v2)
{
  return __riscv_vcreate_v_i8m2x3 (v0, v1, v2);
}

vint8m2x4_t
test_vcreate_v_i8m2x4 (vint8m2_t v0, vint8m2_t v1, vint8m2_t v2, vint8m2_t v3)
{
  return __riscv_vcreate_v_i8m2x4 (v0, v1, v2, v3);
}

vint8m4x2_t
test_vcreate_v_i8m4x2 (vint8m4_t v0, vint8m4_t v1)
{
  return __riscv_vcreate_v_i8m4x2 (v0, v1);
}

vint16m4x2_t
test_vcreate_v_i16m4x2 (vint16m4_t v0, vint16m4_t v1)
{
  return __riscv_vcreate_v_i16m4x2 (v0, v1);
}

vint32m4x2_t
test_vcreate_v_i32m4x2 (vint32m4_t v0, vint32m4_t v1)
{
  return __riscv_vcreate_v_i32m4x2 (v0, v1);
}

vint64m2x2_t
test_vcreate_v_i64m2x2 (vint64m2_t v0, vint64m2_t v1)
{
  return __riscv_vcreate_v_i64m2x2 (v0, v1);
}

vint64m2x3_t
test_vcreate_v_i64m2x3 (vint64m2_t v0, vint64m2_t v1, vint64m2_t v2)
{
  return __riscv_vcreate_v_i64m2x3 (v0, v1, v2);
}

vint64m2x4_t
test_vcreate_v_i64m2x4 (vint64m2_t v0, vint64m2_t v1, vint64m2_t v2,
			vint64m2_t v3)
{
  return __riscv_vcreate_v_i64m2x4 (v0, v1, v2, v3);
}

// Ideally with O3, should find 0 instances of any vmvnr.v PR113913
/* { dg-final { scan-assembler-times {vmv1r.v\s+v[0-9]+,\s*v[0-9]+} 72 } } */
/* { dg-final { scan-assembler-times {vmv2r.v\s+v[0-9]+,\s*v[0-9]+} 36 } } */
/* { dg-final { scan-assembler-times {vmv4r.v\s+v[0-9]+,\s*v[0-9]+} 16 } } */
