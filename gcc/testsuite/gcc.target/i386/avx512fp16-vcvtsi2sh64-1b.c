/* { dg-do run { target { { ! ia32 } && avx512fp16 } } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"

#define N_ELEMS 8

void NOINLINE
emulate_vcvtsi2sh(V512 *dest, V512 op1, 
		  int value_32, __int64_t value_64, int bits)
{
  V512 v1,v2,v5,v6;
  unpack_ph_2twops(op1, &v1, &v2);
  if (bits == 32)
    v5.xmm[0] = _mm_cvt_roundi32_ss (v1.xmm[0], value_32, _ROUND_NINT);
#ifdef __x86_64__
  else 
    v5.xmm[0] = _mm_cvt_roundi64_ss (v1.xmm[0], value_64, _ROUND_NINT);
#endif
  v5.xmm[1] = v1.xmm[1]; 
  *dest = pack_twops_2ph(v5, v6);
}

void
test_512 (void)
{
  V512 res;
  V512 exp;
  
  init_src();
  emulate_vcvtsi2sh(&exp, src1, 0, 99, 64);
  res.xmmh[0] = _mm_cvt_roundi64_sh(src1.xmmh[0], 99, _ROUND_NINT);
  check_results(&res, &exp, N_ELEMS, "_mm_cvt_roundi64_sh");

  if (n_errs != 0) {
      abort ();
  }
}

