/* { dg-do run  { target { { ! ia32 } && avx512fp16 } } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 4

void NOINLINE
emulate_cvtph2_q(V512 * dest, V512 op1,
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
      v5.u64[i] = v1.f32[i];
    }
  }
  *dest = v5;
}

void
test_512 (void)
{
  V512 res;
  V512 exp;
  
  init_src();
  emulate_cvtph2_q(&exp, src1,  NET_MASK, 0);
  res.u64[0] = _mm_cvtt_roundsh_i64(src1.xmmh[0], _ROUND_NINT);
  check_results(&res, &exp, 4, "_mm_cvtt_roundsh_u64");

  if (n_errs != 0) {
      abort ();
  }
}

