/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3" } */

#include "riscv_vector.h"

void
foo (float *r, const float *x)
{
  int i, k;

  vfloat32m4_t x_vec;
  vfloat32m4_t x_forward_vec;
  vfloat32m4_t temp_vec;
  vfloat32m1_t dst_vec;
  vfloat32m1_t src_vec;

  float result = 0.0f;
  float shift_prev = 0.0f;

  size_t n = 64;
  for (size_t vl; n > 0; n -= vl)
    {
      vl = __riscv_vsetvl_e32m4 (n);
      x_vec = __riscv_vle32_v_f32m4 (&x[0], vl);
      x_forward_vec = __riscv_vle32_v_f32m4 (&x[0], vl);
      temp_vec = __riscv_vfmul_vv_f32m4 (x_vec, x_forward_vec, vl);
      src_vec = __riscv_vfmv_s_tu (src_vec, 0.0f, vl);
      dst_vec = __riscv_vfmv_s_tu (dst_vec, 0.0f, vl);
      dst_vec = __riscv_vfredosum_tu (dst_vec, temp_vec, src_vec, vl);
      r[0] = __riscv_vfmv_f_s_f32m1_f32 (dst_vec);
    }
}

/* { dg-final { scan-assembler-times {vsetvli} 1 } } */
/* { dg-final { scan-assembler-not {vsetivli} } } */
/* { dg-final { scan-assembler-times {vsetvli\t[a-x0-9]+,\s*[a-x0-9]+,\s*e32,\s*m4,\s*tu,\s*m[au]} 1 } } */
