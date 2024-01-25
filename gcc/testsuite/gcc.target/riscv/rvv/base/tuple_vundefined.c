/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vfloat16mf4x2_t
test_vundefined_f16mf4x2 ()
{
  return __riscv_vundefined_f16mf4x2 ();
}

vfloat32m1x3_t
test_vundefined_f32m1x3 ()
{
  return __riscv_vundefined_f32m1x3 ();
}

vfloat64m1x5_t
test_vundefined_f64m1x5 ()
{
  return __riscv_vundefined_f64m1x5 ();
}

vint8mf4x2_t
test_vundefined_i8mf4x2 ()
{
  return __riscv_vundefined_i8mf4x2 ();
}

vint16mf4x8_t
test_vundefined_i16mf4x8 ()
{
  return __riscv_vundefined_i16mf4x8 ();
}

vint32m1x7_t
test_vundefined_i32m1x7 ()
{
  return __riscv_vundefined_i32m1x7 ();
}

vint64m1x4_t
test_vundefined_i64m1x4 ()
{
  return __riscv_vundefined_i64m1x4 ();
}

vuint8mf8x2_t
test_vundefined_u8mf8x2 ()
{
  return __riscv_vundefined_u8mf8x2 ();
}

vuint16mf4x4_t
test_vundefined_u16mf4x4 ()
{
  return __riscv_vundefined_u16mf4x4 ();
}

vuint32m1x7_t
test_vundefined_u32m1x7 ()
{
  return __riscv_vundefined_u32m1x7 ();
}

vuint64m4x2_t
test_vundefined_u64m4x2 ()
{
  return __riscv_vundefined_u64m4x2 ();
}

/* { dg-final { scan-assembler-times {vse[0-9]+\.v\s+v[0-9]+,\s*0\([0-9ax]+\)} 0 } } */
/* { dg-final { scan-assembler-times {vs[0-9]+r\.v\s+v[0-9]+,\s*0\([a-x][0-9]+\)} 0 } } */
