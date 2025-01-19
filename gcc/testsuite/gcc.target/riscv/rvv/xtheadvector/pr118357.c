/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gc_xtheadvector -mabi=lp64d -O2" } */

#include <riscv_vector.h>

vfloat16m4_t foo (float *ptr, size_t vl)
{
  vfloat32m8_t _p = __riscv_vle32_v_f32m8 (ptr, vl);
  vfloat16m4_t _half = __riscv_vfncvt_f_f_w_f16m4 (_p, vl);
  return _half;
}

/* { dg-final { scan-assembler-not {th.vsetvli\tzero,zero} } }*/
