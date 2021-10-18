/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 16)

void NOINLINE
EMULATE(cvtph2_pd) (V512 * dest, V512 op1,
                 __mmask32 k, int zero_mask)
{
    V512 v1, v2, v3, v4, v5, v6, v7, v8; 
    int i;
    __mmask16 m1, m2;
    
    m1 = k & 0xffff; 
    unpack_ph_2twops(op1, &v1, &v2);

    for (i = 0; i < 8; i++) {
        if (((1 << i) & m1) == 0) {
            if (zero_mask) {
               v5.u64[i] = 0;
            }
            else {
               v5.u64[i] = dest->u64[i];
            }
        }
        else {
           v5.f64[i] = v1.f32[i];
        }
    }

    *dest = v5;
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtph2_pd)(&exp, src1,  NET_MASK, 0);
  DF(res) = INTRINSIC (_cvtph_pd) (src1.xmmh[0]);
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvtph_pd);
 
  init_dest(&res, &exp);
  EMULATE(cvtph2_pd)(&exp, src1, 0xcc, 0);
  DF(res) = INTRINSIC (_mask_cvtph_pd) (DF(res), 0xcc, src1.xmmh[0]);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvtph_pd);
 
  EMULATE(cvtph2_pd)(&exp, src1,  0xc1, 1);
  DF(res) = INTRINSIC (_maskz_cvtph_pd) (0xc1, src1.xmmh[0]);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvtph_pd);

#if AVX512F_LEN == 512
  EMULATE(cvtph2_pd)(&exp, src1,  NET_MASK, 0);
  DF(res) = INTRINSIC (_cvt_roundph_pd) (src1.xmmh[0], _ROUND_CUR);
  CHECK_RESULT (&res, &exp, N_ELEMS, _cvt_roundph_pd);
 
  init_dest(&res, &exp);
  EMULATE(cvtph2_pd)(&exp, src1, 0xcc, 0);
  DF(res) = INTRINSIC (_mask_cvt_roundph_pd) (DF(res), 0xcc, src1.xmmh[0], _ROUND_CUR);
  CHECK_RESULT (&res, &exp, N_ELEMS, _mask_cvt_roundph_pd);
 
  EMULATE(cvtph2_pd)(&exp, src1,  0xc1, 1);
  DF(res) = INTRINSIC (_maskz_cvt_roundph_pd) (0xc1, src1.xmmh[0], _ROUND_CUR);
  CHECK_RESULT (&res, &exp, N_ELEMS, _maskz_cvt_roundph_pd);
#endif

  if (n_errs != 0) {
    abort ();
}
}

