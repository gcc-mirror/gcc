#include "riscv_vector.h"

vint8m1_t test_vadd_vv_i8m1(vint8m1_t vs2, vint8m1_t vs1, size_t vl) {
  return __riscv_vadd(vs2, vs1, vl);
}

vint8m1_t test_vadd_vx_i8m1(vint8m1_t vs2, int8_t rs1, size_t vl) {
  return __riscv_vadd(vs2, rs1, vl);
}

vint8m1_t test_vadd_vv_i8m1_m(vbool8_t vm, vint8m1_t vs2, vint8m1_t vs1,
                              size_t vl) {
  return __riscv_vadd(vm, vs2, vs1, vl);
}

vint8m1_t test_vadd_vx_i8m1_m(vbool8_t vm, vint8m1_t vs2, int8_t rs1,
                              size_t vl) {
  return __riscv_vadd(vm, vs2, rs1, vl);
}

vint8m1_t test_vadd_vv_i8m1_tu(vint8m1_t vd, vint8m1_t vs2, vint8m1_t vs1,
                               size_t vl) {
  return __riscv_vadd_tu(vd, vs2, vs1, vl);
}

vint8m1_t test_vadd_vx_i8m1_tu(vint8m1_t vd, vint8m1_t vs2, int8_t rs1,
                               size_t vl) {
  return __riscv_vadd_tu(vd, vs2, rs1, vl);
}

vint8m1_t test_vadd_vv_i8m1_tum(vbool8_t vm, vint8m1_t vd, vint8m1_t vs2,
                                vint8m1_t vs1, size_t vl) {
  return __riscv_vadd_tum(vm, vd, vs2, vs1, vl);
}

vint8m1_t test_vadd_vx_i8m1_tum(vbool8_t vm, vint8m1_t vd, vint8m1_t vs2,
                                int8_t rs1, size_t vl) {
  return __riscv_vadd_tum(vm, vd, vs2, rs1, vl);
}

vint8m1_t test_vadd_vv_i8m1_mu(vbool8_t vm, vint8m1_t vd, vint8m1_t vs2,
                               vint8m1_t vs1, size_t vl) {
  return __riscv_vadd_mu(vm, vd, vs2, vs1, vl);
}

vint8m1_t test_vadd_vx_i8m1_mu(vbool8_t vm, vint8m1_t vd, vint8m1_t vs2,
                               int8_t rs1, size_t vl) {
  return __riscv_vadd_mu(vm, vd, vs2, rs1, vl);
}

vint8m1_t test_vadd_vv_i8m1_tumu(vbool8_t vm, vint8m1_t vd, vint8m1_t vs2,
                                 vint8m1_t vs1, size_t vl) {
  return __riscv_vadd_tumu(vm, vd, vs2, vs1, vl);
}

vint8m1_t test_vadd_vx_i8m1_tumu(vbool8_t vm, vint8m1_t vd, vint8m1_t vs2,
                                 int8_t rs1, size_t vl) {
  return __riscv_vadd_tumu(vm, vd, vs2, rs1, vl);
}
