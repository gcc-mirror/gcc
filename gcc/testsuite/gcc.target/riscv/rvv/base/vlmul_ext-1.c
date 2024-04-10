/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include <riscv_vector.h>

vint16m8_t test_vlmul_ext_v_i16mf4_i16m8(vint16mf4_t op1) {
  return __riscv_vlmul_ext_v_i16mf4_i16m8(op1);
}

vint64m8_t test_vlmul_ext_v_i64m2_i64m8(vint64m2_t op1) {
  return __riscv_vlmul_ext_v_i64m2_i64m8(op1);
}

/* { dg-final { scan-assembler-times {vmv1r.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vmv2r.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
