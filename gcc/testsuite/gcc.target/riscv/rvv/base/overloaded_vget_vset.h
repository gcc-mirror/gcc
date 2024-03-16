#include "riscv_vector.h"

vfloat16m1_t test_vget_v_f16m2_f16m1(vfloat16m2_t src, size_t index) {
  return __riscv_vget_f16m1(src, 0);
}

vint64m1_t test_vget_v_i64m4_i64m1(vint64m4_t src, size_t index) {
  return __riscv_vget_i64m1(src, 0);
}

vfloat16m1_t test_vget_v_f16m1x4_f16m1(vfloat16m1x4_t src, size_t index) {
  return __riscv_vget_f16m1(src, 0);
}

vint8m2_t test_vget_v_i8m2x3_i8m2(vint8m2x3_t src, size_t index) {
  return __riscv_vget_i8m2(src, 0);
}

vfloat16m2_t test_vset_v_f16m1_f16m2(vfloat16m2_t dest, size_t index,
                                     vfloat16m1_t value) {
  return __riscv_vset(dest, 0, value);
}

vfloat64m1x7_t test_vset_v_f64m1_f64m1x7(vfloat64m1x7_t dest, size_t index,
                                         vfloat64m1_t value) {
  return __riscv_vset(dest, 0, value);
}
