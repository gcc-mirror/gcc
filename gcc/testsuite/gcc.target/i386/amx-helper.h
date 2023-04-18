#ifndef AMX_HELPER_H_INCLUDED
#define AMX_HELPER_H_INCLUDED
#if defined(AMX_FP16) || defined(AMX_COMPLEX)
#include <immintrin.h>
#include <xmmintrin.h>
#endif
#include "amx-check.h"

typedef union
{
  _Float16 f16;
  uint16_t u;
} union16f_uw;

#if defined(AMX_FP16) || defined(AMX_COMPLEX)
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

#endif
