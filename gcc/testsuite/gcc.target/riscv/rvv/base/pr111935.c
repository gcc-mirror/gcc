/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O0" } */

#include "riscv_vector.h"

inline vuint32m4_t __attribute__((__always_inline__)) transpose_indexes() {
  static const uint32_t idx_[16] = {0, 4, 8, 12,
                      1, 5, 9, 13,
                      2, 6, 10, 14,
                      3, 7, 11, 15};
  return __riscv_vle32_v_u32m4(idx_, 16);
}

void pffft_real_preprocess_4x4(const float *in) {
  vfloat32m1_t r0=__riscv_vle32_v_f32m1(in,4);
  vfloat32m4_t tmp = __riscv_vundefined_f32m4();
  tmp = __riscv_vset_v_f32m1_f32m4(tmp, 0, r0);
  tmp = __riscv_vset_v_f32m1_f32m4(tmp, 1, r0);
  tmp = __riscv_vset_v_f32m1_f32m4(tmp, 2, r0);
  tmp = __riscv_vset_v_f32m1_f32m4(tmp, 3, r0);
  tmp = __riscv_vrgather_vv_f32m4(tmp, transpose_indexes(), 16);
  r0 = __riscv_vget_v_f32m4_f32m1(tmp, 0);
}

/* { dg-final { scan-assembler-times {vl[0-9]+re[0-9]+\.v\s+v[0-9]+,\s*0\([a-z]+[0-9]+\)} 10 } } */
/* { dg-final { scan-assembler-times {vs[0-9]+r\.v\s+v[0-9]+,\s*0\([a-z]+[0-9]+\)} 8 } } */
