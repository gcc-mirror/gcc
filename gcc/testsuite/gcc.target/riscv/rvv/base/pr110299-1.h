#include "riscv_vector.h"

vfloat32m1_t test_vfwredosum_vs_f16m8_f32m1(vfloat16m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredosum_vs_f16m8_f32m1(vector, scalar, vl);
}

vfloat32m1_t test_vfwredusum_vs_f16m8_f32m1(vfloat16m8_t vector, vfloat32m1_t scalar, size_t vl) {
  return __riscv_vfwredusum_vs_f16m8_f32m1(vector, scalar, vl);
}
