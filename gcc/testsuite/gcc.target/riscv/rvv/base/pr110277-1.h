#include "riscv_vector.h"

vfloat16m1_t test_vfredmax_vs_f16mf2_f16m1(vfloat16mf2_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredmax_vs_f16mf2_f16m1(vector, scalar, vl);
}

vfloat32m1_t test_vfredmax_vs_f32m8_f32m1(vfloat32m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfredmax_vs_f32m8_f32m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredmin_vs_f16mf2_f16m1(vfloat16mf2_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredmin_vs_f16mf2_f16m1(vector, scalar, vl);
}

vfloat32m1_t test_vfredmin_vs_f32m8_f32m1(vfloat32m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfredmin_vs_f32m8_f32m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredosum_vs_f16mf2_f16m1(vfloat16mf2_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredosum_vs_f16mf2_f16m1(vector, scalar, vl);
}

vfloat32m1_t test_vfredosum_vs_f32m8_f32m1(vfloat32m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfredosum_vs_f32m8_f32m1(vector, scalar, vl);
}

vfloat16m1_t test_vfredusum_vs_f16mf2_f16m1(vfloat16mf2_t vector, vfloat16m1_t scalar, size_t vl) {
  return __riscv_vfredusum_vs_f16mf2_f16m1(vector, scalar, vl);
}

vfloat32m1_t test_vfredusum_vs_f32m8_f32m1(vfloat32m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfredusum_vs_f32m8_f32m1(vector, scalar, vl);
}
