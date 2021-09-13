/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */

#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

void NOINLINE
EMULATE(mul_ph) (V512 * dest, V512 op1, V512 op2, 
                __mmask32 k, int zero_mask)
{
    V512 v1, v2, v3, v4, v5, v6, v7, v8;
    int i;
    __mmask16 m1, m2;

    m1 = k & 0xffff;
    m2 = (k >> 16) & 0xffff;

    unpack_ph_2twops(op1, &v1, &v2);
    unpack_ph_2twops(op2, &v3, &v4);
    unpack_ph_2twops(*dest, &v7, &v8);

    for (i = 0; i < 16; i++) {
        if (((1 << i) & m1) == 0) {
            if (zero_mask) {
               v5.f32[i] = 0;
            }
            else {
               v5.u32[i] = v7.u32[i];
            }
        }
        else {
           v5.f32[i] = v1.f32[i] * v3.f32[i];
        }

        if (((1 << i) & m2) == 0) {
            if (zero_mask) {
               v6.f32[i] = 0;
            }
            else {
               v6.u32[i] = v8.u32[i];
            }
        }
        else {
            v6.f32[i] = v2.f32[i] * v4.f32[i];
        }

    }
    *dest = pack_twops_2ph(v5, v6);
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(mul_ph) (&exp, src1, src2, NET_MASK, 0);
  HF(res) = INTRINSIC (_mul_ph) (HF(src1), HF(src2));
  CHECK_RESULT (&res, &exp, N_ELEMS, _mul_ph);

  init_dest(&res, &exp);
  EMULATE(mul_ph) (&exp, src1, src2, MASK_VALUE, 0);
  HF(res) = INTRINSIC (_mask_mul_ph) (HF(res), MASK_VALUE, HF(src1), HF(src2));
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_mul_ph);

  EMULATE(mul_ph) (&exp, src1, src2, ZMASK_VALUE, 1);
  HF(res) = INTRINSIC (_maskz_mul_ph) (ZMASK_VALUE, HF(src1), HF(src2));
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_mul_ph);

#if AVX512F_LEN == 512
  EMULATE(mul_ph) (&exp, src1, src2, NET_MASK, 0);
  HF(res) = INTRINSIC (_mul_round_ph) (HF(src1), HF(src2), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mul_ph);

  init_dest(&res, &exp);
  EMULATE(mul_ph) (&exp, src1, src2, MASK_VALUE, 0);
  HF(res) = INTRINSIC (_mask_mul_round_ph) (HF(res), MASK_VALUE, HF(src1), HF(src2), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_mul_ph);
  
  EMULATE(mul_ph) (&exp, src1, src2, ZMASK_VALUE, 1);
  HF(res) = INTRINSIC (_maskz_mul_round_ph) (ZMASK_VALUE, HF(src1), HF(src2), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_mul_ph);
#endif

  if (n_errs != 0) {
      abort ();
  }
}
