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

vbool2_t test_vreinterpret_v_i8m1_b2 (vint8m1_t src) {
  return __riscv_vreinterpret_v_i8m1_b2 (src);
}

vbool4_t test_vreinterpret_v_i8m1_b4 (vint8m1_t src) {
  return __riscv_vreinterpret_v_i8m1_b4 (src);
}

vbool8_t test_vreinterpret_v_i8m1_b8 (vint8m1_t src) {
  return __riscv_vreinterpret_v_i8m1_b8 (src);
}

vbool16_t test_vreinterpret_v_i8m1_b16 (vint8m1_t src) {
  return __riscv_vreinterpret_v_i8m1_b16 (src);
}

vbool32_t test_vreinterpret_v_i8m1_b32 (vint8m1_t src) {
  return __riscv_vreinterpret_v_i8m1_b32 (src);
}

vbool64_t test_vreinterpret_v_i8m1_b64 (vint8m1_t src) {
  return __riscv_vreinterpret_v_i8m1_b64 (src);
}

vbool2_t test_vreinterpret_v_u8m1_b2 (vuint8m1_t src) {
  return __riscv_vreinterpret_v_u8m1_b2 (src);
}

vbool4_t test_vreinterpret_v_u8m1_b4 (vuint8m1_t src) {
  return __riscv_vreinterpret_v_u8m1_b4 (src);
}

vbool8_t test_vreinterpret_v_u8m1_b8 (vuint8m1_t src) {
  return __riscv_vreinterpret_v_u8m1_b8 (src);
}

vbool16_t test_vreinterpret_v_u8m1_b16 (vuint8m1_t src) {
  return __riscv_vreinterpret_v_u8m1_b16 (src);
}

vbool32_t test_vreinterpret_v_u8m1_b32 (vuint8m1_t src) {
  return __riscv_vreinterpret_v_u8m1_b32 (src);
}

vbool64_t test_vreinterpret_v_u8m1_b64 (vuint8m1_t src) {
  return __riscv_vreinterpret_v_u8m1_b64 (src);
}

vint8m1_t test_vreinterpret_v_b1_vint8m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_i8m1 (src);
}

vint16m1_t test_vreinterpret_v_b1_vint16m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_i16m1 (src);
}

vint32m1_t test_vreinterpret_v_b1_vint32m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_i32m1 (src);
}

vint64m1_t test_vreinterpret_v_b1_vint64m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_i64m1 (src);
}

vuint8m1_t test_vreinterpret_v_b1_vuint8m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_u8m1 (src);
}

vuint16m1_t test_vreinterpret_v_b1_vuint16m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_u16m1 (src);
}

vuint32m1_t test_vreinterpret_v_b1_vuint32m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_u32m1 (src);
}

vuint64m1_t test_vreinterpret_v_b1_vuint64m1 (vbool1_t src) {
  return __riscv_vreinterpret_v_b1_u64m1 (src);
}

/* { dg-final { scan-assembler-times {vmv1r.v\s+v[0-9]+,\s*v[0-9]+} 28 } } */
