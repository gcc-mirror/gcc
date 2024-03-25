#include "riscv_vector.h"

vint8m1_t test_vmv_v_v_i8m1 (vint8m1_t vs1, size_t vl) {
  return __riscv_vmv_v (vs1, vl);
}

vint8m1_t test_vmv_v_v_i8m1_tu (vint8m1_t vd, vint8m1_t vs1, size_t vl) {
  return __riscv_vmv_v_tu(vd, vs1, vl);
}

vfloat16m1_t test_vmv_v_v_f16m1 (vfloat16m1_t vs1, size_t vl) {
  return __riscv_vmv_v (vs1, vl);
}

vfloat16m1_t test_vmv_v_v_f16m1_tu (vfloat16m1_t vd, vfloat16m1_t vs1,
                                   size_t vl) {
  return __riscv_vmv_v_tu (vd, vs1, vl);
}

int8_t test_vmv_x_s_i8m1_i8(vint8m1_t vs1) {
  return __riscv_vmv_x (vs1);
}

vint8m1_t test_vmv_s_x_i8m1_tu(vint8m1_t vd, int8_t rs1, size_t vl) {
  return __riscv_vmv_s_tu(vd, rs1, vl);
}
