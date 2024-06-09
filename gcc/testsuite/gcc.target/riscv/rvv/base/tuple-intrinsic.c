/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3 -fno-schedule-insns -fno-schedule-insns2" } */

#include "riscv_vector.h"

typedef _Float16 float16_t;
typedef double float64_t;

void test_vsoxseg3ei64_v_f16mf4x3(float16_t *base, vuint64m1_t bindex, vfloat16mf4x3_t v_tuple, size_t vl) {
  return __riscv_vsoxseg3ei64_v_f16mf4x3(base, bindex, v_tuple, vl);
}

void test_vsoxseg3ei32_v_i32mf2x3(int32_t *base, vuint32mf2_t bindex, vint32mf2x3_t v_tuple, size_t vl) {
  return __riscv_vsoxseg3ei32_v_i32mf2x3(base, bindex, v_tuple, vl);
}

void test_vsoxseg4ei16_v_i32mf2x4(int32_t *base, vuint16mf4_t bindex, vint32mf2x4_t v_tuple, size_t vl) {
  return __riscv_vsoxseg4ei16_v_i32mf2x4(base, bindex, v_tuple, vl);
}

vfloat64m4x2_t test_vloxseg2ei8_v_f64m4x2(const float64_t *base, vuint8mf2_t bindex, size_t vl) {
  return __riscv_vloxseg2ei8_v_f64m4x2(base, bindex, vl);
}
