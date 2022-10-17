/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS (AVX512F_LEN / 64)

void NOINLINE
EMULATE(cvtpd2_ph) (V512 * dest, V512 op1, int n_el,
                 __mmask32 k, int zero_mask)
{
    V512 v1, v2, v3, v4, v5, v6, v7, v8;
    int i;
    __mmask16 m1, m2;

    m1 = k & 0xffff;
    unpack_ph_2twops(*dest, &v7, &v8);

    for (i = 0; i < n_el; i++) {
        if (((1 << i) & m1) == 0) {
            if (zero_mask) {
               v5.f32[i] = 0;
            }
            else {
               v5.u32[i] = v7.u32[i];
            }
        }
        else {
           v5.f32[i] = op1.f64[i];
        }
    }
    *dest = pack_twops_2ph(v5, v5);
    for (i = n_el; i < 8; i++)
      dest->u16[i] = 0;
}

void
TEST (void)
{
  V512 res;
  V512 exp;

  init_src();

  EMULATE(cvtpd2_ph)(&exp, src3f, N_ELEMS, NET_MASK, 0);
  res.xmmh[0] = INTRINSIC (_cvtpd_ph) (DF(src3f));
  CHECK_RESULT (&res, &exp, 8, _cvtpd_ph);

  init_dest(&res, &exp);
  EMULATE(cvtpd2_ph)(&exp, src3f, N_ELEMS, 0xcc, 0);
  res.xmmh[0] = INTRINSIC (_mask_cvtpd_ph) (res.xmmh[0], 0xcc,
					   DF(src3f));
  CHECK_RESULT (&res, &exp, 8, _mask_cvtpd_ph);

  EMULATE(cvtpd2_ph)(&exp, src3f, N_ELEMS, 0xf1, 1);
  res.xmmh[0] = INTRINSIC (_maskz_cvtpd_ph) (0xf1, DF(src3f));
  CHECK_RESULT (&res, &exp, 8, _maskz_cvtpd_ph);

#if AVX512F_LEN == 512
  EMULATE(cvtpd2_ph)(&exp, src3f, N_ELEMS, NET_MASK, 0);
  res.xmmh[0] = INTRINSIC (_cvt_roundpd_ph) (DF(src3f), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, 8, _cvt_roundpd_ph);

  init_dest(&res, &exp);
  EMULATE(cvtpd2_ph)(&exp, src3f, N_ELEMS, 0xcc, 0);
  res.xmmh[0] = INTRINSIC (_mask_cvt_roundpd_ph) (res.xmmh[0], 0xcc,
					   DF(src3f), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, 8, _mask_cvt_roundpd_ph);

  EMULATE(cvtpd2_ph)(&exp, src3f, N_ELEMS, 0xf1, 1);
  res.xmmh[0] = INTRINSIC (_maskz_cvt_roundpd_ph) (0xf1, DF(src3f), _ROUND_NINT);
  CHECK_RESULT (&res, &exp, 8, _maskz_cvt_roundpd_ph);
#endif

  if (n_errs != 0) {
      abort ();
  }
}


