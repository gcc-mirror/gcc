/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64 -O3" } */

#include "riscv_vector.h"

vfloat16mf4_t test_vfncvt_f_f_w_f16mf4(vfloat32mf2_t src, size_t vl) {
  return __riscv_vfncvt_f_f_w_f16mf4(src, vl);
}

vfloat16mf2_t test_vfncvt_f_f_w_f16mf2(vfloat32m1_t src, size_t vl) {
  return __riscv_vfncvt_f_f_w_f16mf2(src, vl);
}

vfloat16m1_t test_vfncvt_f_f_w_f16m1(vfloat32m2_t src, size_t vl) {
  return __riscv_vfncvt_f_f_w_f16m1(src, vl);
}

vfloat16m2_t test_vfncvt_f_f_w_f16m2(vfloat32m4_t src, size_t vl) {
  return __riscv_vfncvt_f_f_w_f16m2(src, vl);
}

vfloat16m4_t test_vfncvt_f_f_w_f16m4(vfloat32m8_t src, size_t vl) {
  return __riscv_vfncvt_f_f_w_f16m4(src, vl);
}

vfloat32mf2_t test_vfwcvt_f_f_v_f32mf2(vfloat16mf4_t src, size_t vl) {
  return __riscv_vfwcvt_f_f_v_f32mf2(src, vl);
}

vfloat32m1_t test_vfwcvt_f_f_v_f32m1(vfloat16mf2_t src, size_t vl) {
  return __riscv_vfwcvt_f_f_v_f32m1(src, vl);
}

vfloat32m2_t test_vfwcvt_f_f_v_f32m2(vfloat16m1_t src, size_t vl) {
  return __riscv_vfwcvt_f_f_v_f32m2(src, vl);
}

vfloat32m4_t test_vfwcvt_f_f_v_f32m4(vfloat16m2_t src, size_t vl) {
  return __riscv_vfwcvt_f_f_v_f32m4(src, vl);
}

vfloat32m8_t test_vfwcvt_f_f_v_f32m8(vfloat16m4_t src, size_t vl) {
  return __riscv_vfwcvt_f_f_v_f32m8(src, vl);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.f\.v\s+v[0-9]+,\s*v[0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {vfncvt\.f\.f\.w\s+v[0-9]+,\s*v[0-9]+} 5 } } */

