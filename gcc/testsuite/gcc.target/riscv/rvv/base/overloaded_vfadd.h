#include "riscv_vector.h"

vfloat16mf4_t test_vfadd_vv_f16mf4(vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                   size_t vl) {
  return __riscv_vfadd(vs2, vs1, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_m(vbool64_t vm, vfloat16mf4_t vs2,
                                     vfloat16mf4_t vs1, size_t vl) {
  return __riscv_vfadd(vm, vs2, vs1, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_tu(vfloat16mf4_t vd, vfloat16mf4_t vs2,
                                      vfloat16mf4_t vs1, size_t vl) {
  return __riscv_vfadd_tu(vd, vs2, vs1, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_tum(vbool64_t vm, vfloat16mf4_t vd,
                                       vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                       size_t vl) {
  return __riscv_vfadd_tum(vm, vd, vs2, vs1, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_tumu(vbool64_t vm, vfloat16mf4_t vd,
                                        vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                        size_t vl) {
  return __riscv_vfadd_tumu(vm, vd, vs2, vs1, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_mu(vbool64_t vm, vfloat16mf4_t vd,
                                      vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                      size_t vl) {
  return __riscv_vfadd_mu(vm, vd, vs2, vs1, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_rm(vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                      size_t vl) {
  return __riscv_vfadd(vs2, vs1, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_rm_m(vbool64_t vm, vfloat16mf4_t vs2,
                                        vfloat16mf4_t vs1, size_t vl) {
  return __riscv_vfadd(vm, vs2, vs1, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_rm_tu(vfloat16mf4_t vd, vfloat16mf4_t vs2,
                                         vfloat16mf4_t vs1, size_t vl) {
  return __riscv_vfadd_tu(vd, vs2, vs1, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_rm_tum(vbool64_t vm, vfloat16mf4_t vd,
                                          vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                          size_t vl) {
  return __riscv_vfadd_tum(vm, vd, vs2, vs1, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_rm_tumu(vbool64_t vm, vfloat16mf4_t vd,
                                           vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                           size_t vl) {
  return __riscv_vfadd_tumu(vm, vd, vs2, vs1, __RISCV_FRM_RNE, vl);
}

vfloat16mf4_t test_vfadd_vv_f16mf4_rm_mu(vbool64_t vm, vfloat16mf4_t vd,
                                         vfloat16mf4_t vs2, vfloat16mf4_t vs1,
                                         size_t vl) {
  return __riscv_vfadd_mu(vm, vd, vs2, vs1, __RISCV_FRM_RNE, vl);
}
