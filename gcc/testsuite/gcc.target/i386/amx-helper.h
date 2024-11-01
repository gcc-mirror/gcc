#ifndef AMX_HELPER_H_INCLUDED
#define AMX_HELPER_H_INCLUDED
#include <immintrin.h>
#include <xmmintrin.h>
#include "amx-check.h"

typedef union
{
  _Float16 f16;
  uint16_t u;
} union16f_uw;

typedef union
{
  __bf16 bf16;
  uint16_t u;
} union16bh_uw;

typedef union
{
  float f;
  uint32_t u;
} union32f_ud;

typedef union
{
  __m512 m;
  uint8_t u[64];
} union512_ub;

#if defined(AMX_FP16) || defined(AMX_COMPLEX) || defined (AMX_AVX512)
/* Transformation functions between fp16/float */
static uint16_t make_f32_fp16 (float f)
{
  union16f_uw tmp;
  __m128 b = _mm_set_ss (f);
  __m128h a;
  tmp.f16 = _mm_cvtsh_h (_mm_cvtss_sh (a, b));
  return tmp.u;
}

static float make_fp16_f32 (uint16_t fp)
{
  union16f_uw tmp;
  tmp.u = fp;
  __m128h b = _mm_set_sh (tmp.f16);
  __m128 a;
  return _mm_cvtss_f32 (_mm_cvtsh_ss (a, b));
}

/* Init tile buffer with fp16 pairs */
void init_fp16_max_tile_buffer (uint8_t* buf)
{
  int i, j;
  uint16_t* ptr = (uint16_t *) buf;

  for (i = 0; i < 16; i++)
    for (j = 0; j < 32; j++)
    {
      float f = 2.5f * i + 1.25f * j;
      ptr[i * 32 + j] = make_f32_fp16 (f);
    }
}

/* Init tile fp16 pair buffer with zero */
void init_fp16_max_tile_zero_buffer (uint8_t* buf)
{
  int i, j;
  uint16_t* ptr = (uint16_t *) buf;

  for (i = 0; i < 16; i++)
    for (j = 0; j < 32; j++)
      ptr[i * 32 + j] = make_f32_fp16 (0.0f);
}
#endif

#if defined (AMX_AVX512)
/* Transformation functions between bf16/float */
static uint16_t make_f32_bf16 (float f)
{
  union16bh_uw tmp;
  tmp.bf16 = (__bf16) f;
  return tmp.u;
}

static float make_bf16_f32 (uint16_t bf)
{
  union16bh_uw tmp;
  tmp.u = bf;
  return _mm_cvtsbh_ss (tmp.bf16);
}

/* Init tile buffer with bf16 pairs */
void init_bf16_max_tile_buffer (uint8_t *buf)
{
  int i, j;
  uint16_t* ptr = (uint16_t *) buf;

  for (i = 0; i < 16; i++)
    for (j = 0; j < 32; j++)
    {
      float f = 2.5f * i + 1.25f * j;
      ptr[i * 32 + j] = make_f32_bf16 (f);
    }
}
#endif

/* Init tile buffer with fp32 */
void init_fp32_max_tile_buffer (uint8_t *buf)
{
  int i, j;
  float* ptr = (float *) buf;

  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      ptr[i * 16 + j] = 2.5f * i + 1.25f * j;
}

/* Init tile buffer with int32 */
void init_int32_max_tile_buffer (uint8_t *buf)
{
  int i, j;
  uint32_t *ptr = (uint32_t *)buf;

  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      ptr[i * 16 + j] = (uint32_t) (3 * j - 16 * i);
}

#define COMPARE_ZMM(A, B)	\
for (int j = 0; j < 16; j++)	\
{				\
  union32f_ud fu1, fu2;		\
  fu1.f = A[j];			\
  fu2.f = B[j];			\
  if (fu1.u != fu2.u)		\
    abort ();			\
}

#define COMPARE_ZMM_BF16(A, B)	\
for (int j = 0; j < 32; j++)	\
{				\
  union16bh_uw fu1, fu2;	\
  fu1.bf16 = A[j];		\
  fu2.bf16 = B[j];		\
  if (fu1.u != fu2.u)		\
    abort();			\
}

#define COMPARE_ZMM_FP16(A, B)	\
for (int j = 0; j < 32; j++)	\
{				\
  union16f_uw fu1, fu2;		\
  fu1.f16 = A[j];		\
  fu2.f16 = B[j];		\
  if (fu1.u != fu2.u)		\
    abort();			\
}

/* Mask low 13bits to zero */
static float zero_lower_mantissa_bits_fp32 (float x)
{
  union32f_ud tmp;
  tmp.f = x;
  tmp.u = tmp.u & 0xffffe000;
  return tmp.f;
}

/* Handle SNAN */
static float silence_snan_fp32 (float x)
{
  union32f_ud tmp;
  tmp.f = x;
  if ((((tmp.u & 0x7f800000) >> 23) == 0xff) &&
      ((tmp.u & 0x007fffff) != 0) &&
      ((tmp.u & 0x00400000) == 0))
    tmp.u = tmp.u | 0x00400000;
  return tmp.f;
}

#endif
