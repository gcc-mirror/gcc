/* { dg-do compile { target { rv64 } } } */
/* { dg-options "-march=rv64gcv_zvfh -mabi=lp64d -O3" } */

#include "riscv_vector.h"

_Float16 a (uint64_t);
int8_t b () {
  int c = 100;
  double *d;
  _Float16 *e;
  for (size_t f;; c -= f)
    {
      f = c;
      __riscv_vsll_vx_u8mf8 (__riscv_vid_v_u8mf8 (f), 2, f);
      vfloat16mf4_t g;
      a (1);
      g = __riscv_vfmv_s_f_f16mf4 (2, f);
      vfloat64m1_t i = __riscv_vfmv_s_f_f64m1 (30491, f);
      vuint16mf4_t j;
      __riscv_vsoxei16_v_f16mf4 (e, j, g, f);
      vuint8mf8_t k = __riscv_vsll_vx_u8mf8 (__riscv_vid_v_u8mf8 (f), 3, f);
      __riscv_vsoxei8_v_f64m1 (d, k, i, f);
    }
}

/* { dg-final { scan-assembler-not "e64,mf4" } } */
