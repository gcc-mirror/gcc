/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64 -O3" } */
#include "riscv_vector.h"

vbool1_t test_vreinterpret_v_i8m1_b1 (vint8m1_t src) {
  return __riscv_vreinterpret_v_i8m1_b1 (src);
}

vbool1_t test_vreinterpret_v_i16m1_b1 (vint16m1_t src) {
  return __riscv_vreinterpret_v_i16m1_b1 (src);
}

vbool1_t test_vreinterpret_v_i32m1_b1 (vint32m1_t src) {
  return __riscv_vreinterpret_v_i32m1_b1 (src);
}

vbool1_t test_vreinterpret_v_i64m1_b1 (vint64m1_t src) {
  return __riscv_vreinterpret_v_i64m1_b1 (src);
}

vbool1_t test_vreinterpret_v_u8m1_b1 (vuint8m1_t src) {
  return __riscv_vreinterpret_v_u8m1_b1 (src);
}

vbool1_t test_vreinterpret_v_u16m1_b1 (vuint16m1_t src) {
  return __riscv_vreinterpret_v_u16m1_b1 (src);
}

vbool1_t test_vreinterpret_v_u32m1_b1 (vuint32m1_t src) {
  return __riscv_vreinterpret_v_u32m1_b1 (src);
}

vbool1_t test_vreinterpret_v_u64m1_b1 (vuint64m1_t src) {
  return __riscv_vreinterpret_v_u64m1_b1 (src);
}

/* { dg-final { scan-assembler-times {vlm\.v\s+v[0-9]+,\s*0\([a-x][0-9]+\)} 8 } } */
/* { dg-final { scan-assembler-times {vsm\.v\s+v[0-9]+,\s*0\([a-x][0-9]+\)} 8 } } */
