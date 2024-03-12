#include "riscv_vector.h"

vfloat32m1_t test_vfwredosum_vs_f16mf4_f32m1(vfloat16mf4_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredosum_vs_f16mf4_f32m1(vector, scalar, vl);
}

vfloat32m1_t test_vfwredusum_vs_f16mf4_f32m1(vfloat16mf4_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredusum_vs_f16mf4_f32m1(vector, scalar, vl);
}

vfloat64m1_t test_vfwredusum_vs_f32m8_f64m1(vfloat32m8_t vector, vfloat64m1_t scalar, size_t vl) {
  return __riscv_vfwredusum_vs_f32m8_f64m1(vector, scalar, vl);
}

vfloat64m1_t test_vfwredosum_vs_f32m8_f64m1(vfloat32m8_t vector, vfloat64m1_t scalar, size_t vl) {
  return __riscv_vfwredosum_vs_f32m8_f64m1(vector, scalar, vl);
}
