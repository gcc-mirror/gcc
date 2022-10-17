/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 2

void NOINLINE
emulate_cvtph2_d(V512 * dest, V512 op1,
		 __mmask32 k, int zero_mask)
{
  V512 v1, v2, v3, v4, v5, v6, v7, v8;
  int i;
  __mmask16 m1, m2;

  m1 = k & 0xffff;

  unpack_ph_2twops(op1, &v1, &v2);

  for (i = 0; i < 16; i++) {
    if (((1 << i) & m1) == 0) {
      if (zero_mask) {
	v5.u32[i] = 0;
      }
      else {
	v5.u32[i] = dest->u32[i];
      }
    }
    else {
      v5.u32[i] = v1.f32[i];

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
  emulate_cvtph2_d(&exp, src1,  NET_MASK, 0);
  res.i32[0] = _mm_cvtt_roundsh_i32(src1.xmmh[0], _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "_mm_cvtt_roundsh_i32");

  if (n_errs != 0) {
      abort ();
  }
}

