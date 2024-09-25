#ifndef AVX10_HELPER_INCLUDED
#define AVX10_HELPER_INCLUDED

#define AVX10
#define AVX512FP16
#define AVX512BF16
#include "avx512f-helper.h"
#include <stdint.h>

#define NOINLINE __attribute__((noinline,noclone))
typedef union
{
  uint32_t int32;
  float flt;
}float_int_t;

float NOINLINE
convert_bf16_to_fp32 (unsigned short bf16)
{
  unsigned int ii = bf16 << 16;
  return *(float*)&ii;
}

unsigned short NOINLINE
convert_fp32_to_bf16 (float fp)
{
  float_int_t fi;
  fi.flt = fp;
  return ((fi.int32 >> 16) & 0xffff);
}

unsigned short NOINLINE
convert_fp32_to_bf16_ne (float fp)
{
  float_int_t fi;
  uint32_t rounding_bias, lsb;

  fi.flt = fp;
  lsb = (fi.int32 >> 16) & 0x1;
  rounding_bias = 0x7fff + lsb;
  fi.int32 += rounding_bias;

  return ((fi.int32 >> 16) & 0xffff);
}

float NOINLINE
scalef (float x, float y)
{
  __m128 px = _mm_load_ss (&x);
  __m128 py = _mm_load_ss (&y);
  __m128 out = _mm_scalef_ss (px, py);
  return _mm_cvtss_f32 (out);
}

float NOINLINE
getexp (float val)
{
    float res;
    __m128 px = _mm_load_ss (&val);
    __m128 mx = _mm_broadcastss_ps (px);
    __m128 out = _mm_getexp_ps (mx);
    return _mm_cvtss_f32 (out);
}

float NOINLINE
rndscale (float val)
{
    __m128 px = _mm_load_ss (&val);
    __m128 mx = _mm_broadcastss_ps (px);
    __m128 out = _mm_roundscale_ps (mx, 0x10);
    return _mm_cvtss_f32 (out);
}

float NOINLINE 
getmant(float val)
{
    __m128 px = _mm_load_ss (&val);
    __m128 mx = _mm_broadcastss_ps (px);
    __m128 out = _mm_getmant_ps (mx, 0, 0);
    return _mm_cvtss_f32 (out);
}

#endif /* AVX10_HELPER_INCLUDED */

/* Intrinsic being tested. It has different deffinitions,
   depending on AVX512F_LEN, so it's outside include guards
   and in undefed away to silence warnings.  */
#if defined INTRINSIC
#undef INTRINSIC
#endif

#if AVX512F_LEN != 128
#define INTRINSIC(NAME) EVAL(_mm, AVX512F_LEN, NAME)
#else
#define INTRINSIC(NAME) _mm ## NAME
#endif
