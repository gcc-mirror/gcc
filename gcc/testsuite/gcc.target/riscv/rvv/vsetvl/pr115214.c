/* { dg-do compile } */
/* { dg-options "-mrvv-vector-bits=scalable -march=rv64gcv -mabi=lp64d -O3 -w" } */
/* { dg-skip-if "" { *-*-* } { "-flto" } } */

#include <riscv_vector.h>

static inline __attribute__(()) int vaddq_f32();
static inline __attribute__(()) int vload_tillz_f32(int nlane) {
  vint32m1_t __trans_tmp_9;
  {
    int __trans_tmp_0 = nlane;
    {
      vint64m1_t __trans_tmp_1;
      vint64m1_t __trans_tmp_2;
      vint64m1_t __trans_tmp_3;
      vint64m1_t __trans_tmp_4;
      if (__trans_tmp_0 == 1) {
        {
          __trans_tmp_3 =
              __riscv_vslideup_vx_i64m1(__trans_tmp_1, __trans_tmp_2, 1, 2);
        }
        __trans_tmp_4 = __trans_tmp_2;
      }
      __trans_tmp_4 = __trans_tmp_3;
      __trans_tmp_9 = __riscv_vreinterpret_v_i64m1_i32m1(__trans_tmp_3);
    }
  }
  return vaddq_f32(__trans_tmp_9); /* { dg-error {RVV type 'vint32m1_t' cannot be passed to an unprototyped function} } */
}

char CFLOAT_add_args[3];
const int *CFLOAT_add_steps;
const int CFLOAT_steps;

__attribute__(()) void CFLOAT_add() {
  char *b_src0 = &CFLOAT_add_args[0], *b_src1 = &CFLOAT_add_args[1],
       *b_dst = &CFLOAT_add_args[2];
  const float *src1 = (float *)b_src1;
  float *dst = (float *)b_dst;
  const int ssrc1 = CFLOAT_add_steps[1] / sizeof(float);
  const int sdst = CFLOAT_add_steps[2] / sizeof(float);
  const int hstep = 4 / 2;
  vfloat32m1x2_t a;
  int len = 255;
  for (; len > 0; len -= hstep, src1 += 4, dst += 4) {
    int b = vload_tillz_f32(len);
    int r = vaddq_f32(a.__val[0], b); /* { dg-error {RVV type '__rvv_float32m1_t' cannot be passed to an unprototyped function} } */
  }
  for (; len > 0; --len, b_src0 += CFLOAT_steps,
                  b_src1 += CFLOAT_add_steps[1], b_dst += CFLOAT_add_steps[2])
    ;
}
