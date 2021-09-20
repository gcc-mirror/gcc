/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512vl -mavx512dq" } */

#define AVX512FP16
#include "avx512fp16-helper.h"

static void
test_512 (void)
{
  V512 v1, v2, v3, v4, exp, res;
  int i;
  init_src();
  
  unpack_ph_2twops(src1, &v1, &v2);
  v1.f32[0] = -v1.f32[0];
  exp = pack_twops_2ph(v1, v2);
  res.zmmh = src1.zmmh;
  res.f16[0] = -res.f16[0];
  check_results(&res, &exp, 32, "neg");

  unpack_ph_2twops(src1, &v1, &v2);
  for (i=0; i<16; i++)
  {
    v1.f32[i] = -v1.f32[i];  
    v2.f32[i] = -v2.f32[i];  
  }
  exp = pack_twops_2ph(v1, v2);
  res.zmmh = -src1.zmmh;
  check_results(&res, &exp, 32, "neg");
  if (n_errs != 0) {
      abort ();
  }
}
