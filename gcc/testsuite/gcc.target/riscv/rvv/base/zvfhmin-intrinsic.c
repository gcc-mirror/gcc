/* { dg-do compile } */
/* { dg-options "-march=rv64gcv_zvfhmin -mabi=lp64 -O3" } */

#include "riscv_vector.h"

typedef _Float16 float16_t;

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

vfloat16mf4_t test_vle16_v_f16mf4(const float16_t *base, size_t vl) {
  return __riscv_vle16_v_f16mf4(base, vl);
}

vfloat16m8_t test_vle16_v_f16m8(const float16_t *base, size_t vl) {
  return __riscv_vle16_v_f16m8(base, vl);
}

vfloat16mf4_t test_vreinterpret_v_i16mf4_f16mf4(vint16mf4_t src) {
  return __riscv_vreinterpret_v_i16mf4_f16mf4(src);
}

vfloat16m8_t test_vreinterpret_v_i16m8_f16m8(vint16m8_t src) {
  return __riscv_vreinterpret_v_i16m8_f16m8(src);
}

vfloat16mf4_t test_vreinterpret_v_u16mf4_f16mf4(vuint16mf4_t src) {
  return __riscv_vreinterpret_v_u16mf4_f16mf4(src);
}

vfloat16m8_t test_vreinterpret_v_u16m8_f16m8(vuint16m8_t src) {
  return __riscv_vreinterpret_v_u16m8_f16m8(src);
}

vint16mf4_t test_vreinterpret_v_f16mf4_i16mf4(vfloat16mf4_t src) {
  return __riscv_vreinterpret_v_f16mf4_i16mf4(src);
}

vint16m8_t test_vreinterpret_v_f16m8_i16m8(vfloat16m8_t src) {
  return __riscv_vreinterpret_v_f16m8_i16m8(src);
}

vuint16mf4_t test_vreinterpret_v_f16mf4_u16mf4(vfloat16mf4_t src) {
  return __riscv_vreinterpret_v_f16mf4_u16mf4(src);
}

vuint16m8_t test_vreinterpret_v_f16m8_u16m8(vfloat16m8_t src) {
  return __riscv_vreinterpret_v_f16m8_u16m8(src);
}

vfloat16mf2_t test_vlmul_ext_v_f16mf4_f16mf2(vfloat16mf4_t op1) {
  return __riscv_vlmul_ext_v_f16mf4_f16mf2(op1);
}

vfloat16m8_t test_vlmul_ext_v_f16m4_f16m8(vfloat16m4_t op1) {
  return __riscv_vlmul_ext_v_f16m4_f16m8(op1);
}

vfloat16m1_t test_vlmul_ext_v_f16mf4_f16m1(vfloat16mf4_t op1) {
  return __riscv_vlmul_ext_v_f16mf4_f16m1(op1);
}

vfloat16m8_t test_vlmul_ext_v_f16m2_f16m8(vfloat16m2_t op1) {
  return __riscv_vlmul_ext_v_f16m2_f16m8(op1);
}

vfloat16m2_t test_vlmul_ext_v_f16mf4_f16m2(vfloat16mf4_t op1) {
  return __riscv_vlmul_ext_v_f16mf4_f16m2(op1);
}

vfloat16m8_t test_vlmul_ext_v_f16m1_f16m8(vfloat16m1_t op1) {
  return __riscv_vlmul_ext_v_f16m1_f16m8(op1);
}

vfloat16m4_t test_vlmul_ext_v_f16mf4_f16m4(vfloat16mf4_t op1) {
  return __riscv_vlmul_ext_v_f16mf4_f16m4(op1);
}

vfloat16m8_t test_vlmul_ext_v_f16mf2_f16m8(vfloat16mf2_t op1) {
  return __riscv_vlmul_ext_v_f16mf2_f16m8(op1);
}

vfloat16m8_t test_vlmul_ext_v_f16mf4_f16m8(vfloat16mf4_t op1) {
  return __riscv_vlmul_ext_v_f16mf4_f16m8(op1);
}

vfloat16mf4_t test_vlmul_trunc_v_f16mf2_f16mf4(vfloat16mf2_t op1) {
  return __riscv_vlmul_trunc_v_f16mf2_f16mf4(op1);
}

vfloat16mf4_t test_vlmul_trunc_v_f16m1_f16mf4(vfloat16m1_t op1) {
  return __riscv_vlmul_trunc_v_f16m1_f16mf4(op1);
}

vfloat16mf2_t test_vlmul_trunc_v_f16m1_f16mf2(vfloat16m1_t op1) {
  return __riscv_vlmul_trunc_v_f16m1_f16mf2(op1);
}

vfloat16mf4_t test_vlmul_trunc_v_f16m2_f16mf4(vfloat16m2_t op1) {
  return __riscv_vlmul_trunc_v_f16m2_f16mf4(op1);
}

vfloat16m1_t test_vlmul_trunc_v_f16m2_f16m1(vfloat16m2_t op1) {
  return __riscv_vlmul_trunc_v_f16m2_f16m1(op1);
}

vfloat16mf4_t test_vlmul_trunc_v_f16m4_f16mf4(vfloat16m4_t op1) {
  return __riscv_vlmul_trunc_v_f16m4_f16mf4(op1);
}

vfloat16m2_t test_vlmul_trunc_v_f16m4_f16m2(vfloat16m4_t op1) {
  return __riscv_vlmul_trunc_v_f16m4_f16m2(op1);
}

vfloat16mf4_t test_vlmul_trunc_v_f16m8_f16mf4(vfloat16m8_t op1) {
  return __riscv_vlmul_trunc_v_f16m8_f16mf4(op1);
}

vfloat16m4_t test_vlmul_trunc_v_f16m8_f16m4(vfloat16m8_t op1) {
  return __riscv_vlmul_trunc_v_f16m8_f16m4(op1);
}

vfloat16mf4_t test_vundefined_f16mf4() {
  return __riscv_vundefined_f16mf4();
}

vfloat16m8_t test_vundefined_f16m8() {
  return __riscv_vundefined_f16m8();
}

vfloat16m2_t test_vset_v_f16m1_f16m2(vfloat16m2_t dest, size_t index, vfloat16m1_t val) {
  return __riscv_vset_v_f16m1_f16m2(dest, 0, val);
}

vfloat16m8_t test_vset_v_f16m4_f16m8(vfloat16m8_t dest, size_t index, vfloat16m4_t val) {
  return __riscv_vset_v_f16m4_f16m8(dest, 0, val);
}

vfloat16m1_t test_vget_v_f16m2_f16m1(vfloat16m2_t src, size_t index) {
  return __riscv_vget_v_f16m2_f16m1(src, 0);
}

vfloat16m4_t test_vget_v_f16m8_f16m4(vfloat16m8_t src, size_t index) {
  return __riscv_vget_v_f16m8_f16m4(src, 0);
}

/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf4,\s*t[au],\s*m[au]} 3 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*mf2,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m1,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m2,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m4,\s*t[au],\s*m[au]} 2 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+zero,\s*[a-x0-9]+,\s*e16,\s*m8,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m2,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m4,\s*t[au],\s*m[au]} 1 } } */
/* { dg-final { scan-assembler-times {vsetvli\s+[a-x0-9]+,\s*zero,\s*e16,\s*m8,\s*t[au],\s*m[au]} 5 } } */
/* { dg-final { scan-assembler-times {vfwcvt\.f\.f\.v\s+v[0-9]+,\s*v[0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {vfncvt\.f\.f\.w\s+v[0-9]+,\s*v[0-9]+} 5 } } */
/* { dg-final { scan-assembler-times {vle16\.v\s+v[0-9]+,\s*0\([a-x][0-9]+\)} 2 } } */
