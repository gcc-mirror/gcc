/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef _Float16 float16_t;

vfloat16mf4_t test_vfncvt_f_f_w_f16mf4(vfloat32mf2_t src, size_t vl) {
  return __riscv_vfncvt_f_f_w_f16mf4(src, vl);
}

vfloat16m4_t test_vfncvt_f_f_w_f16m4(vfloat32m8_t src, size_t vl) {
  return __riscv_vfncvt_f_f_w_f16m4(src, vl);
}

vfloat32mf2_t test_vfwcvt_f_f_v_f32mf2(vfloat16mf4_t src, size_t vl) {
  return __riscv_vfwcvt_f_f_v_f32mf2(src, vl);
}

vfloat32m8_t test_vfwcvt_f_f_v_f32m8(vfloat16m4_t src, size_t vl) {
  return __riscv_vfwcvt_f_f_v_f32m8(src, vl);
}

vfloat16mf4_t test_vle16_v_f16mf4(const float16_t *base, size_t vl) {
  return __riscv_vle16_v_f16mf4(base, vl);
}

vfloat16m8_t test_vle16_v_f16m8(const float16_t *base, size_t vl) {
  return __riscv_vle16_v_f16m8(base, vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.f\.v\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vfncvt\.f\.f\.w\s+v[0-9]+,\s*v[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {vle16\.v\s+v[0-9]+,\s*0\([0-9ax]+\)} 3 } } */
