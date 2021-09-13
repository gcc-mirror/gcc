/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_tile } */
/* { dg-require-effective-target amx_bf16 } */
/* { dg-options "-O2 -mamx-tile -mamx-bf16" } */
#include <immintrin.h>

#define AMX_BF16
#define DO_TEST test_amx_bf16_dpbf16ps
void test_amx_bf16_dpbf16ps ();
#include "amx-check.h"

/* Transformation functions between bf16/float */
static uint16_t make_bf16 (float f)
{
  uint32_t u = (uint32_t)f;
  u = (u >> 16) & 0xffff;
  return (uint16_t)u;
}

static float make_f32 (uint16_t bf)
{
  uint32_t u = (uint32_t)(bf << 16);
  return (float)u;
}

/* Init tile buffer with bf16 pairs */
void init_bf16_max_tile_buffer (uint8_t *buf)
{ 
  int i, j;
  uint16_t *ptr = (uint16_t *)buf;

  for(i = 0; i < 16; i++)
    for(j = 0; j < 32; j++)
      {	
	float f = 16.1f * i + 3.4f * j;
	ptr[i * 32 + j] = make_bf16(f);
      }
}

void calc_matrix_dpbf16ps (__tile *dst, __tile *src1, __tile *src2)
{
  uint16_t *src1_buf = (uint16_t *)src1->buf;
  uint16_t *src2_buf = (uint16_t *)src2->buf;
  float *dst_buf = (float *)dst->buf;
  
  int M = src1->rows;
  int N = src1->colsb / 4;
  int K = src2->colsb / 4;
  int i, j, k, t;

  for (i = 0; i < M; i++)
    for (j = 0; j < N; j++)
      for (k = 0; k < K; k++)
	for (t = 0; t < 2; t+=2)
	  {    
	    dst_buf[i * N + k] += 
	      (make_f32(src1_buf[i * 4 * N + 4 * j + t]) *
	      make_f32(src2_buf[j * 4 * K + 4 * k + t])) +
	      (make_f32(src1_buf[i * 4 * N + 4 * j + t + 1]) *
	      make_f32(src2_buf[j * 4 * K + 4 * k + t + 1]));
	  }

}

void test_amx_bf16_dpbf16ps ()
{
  __tilecfg_u cfg;
  __tile dst, dst_ref, src1, src2;
  uint8_t tmp_dst_buf[1024];

  init_bf16_max_tile_buffer (tmp_dst_buf);
  
  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, dst, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (2, src1, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (3, src2, tmp_dst_buf);

  calc_matrix_dpbf16ps (&dst, &src1, &src2);
  
  _tile_dpbf16ps (1, 2, 3);
  _tile_stored (1, dst_ref.buf, _STRIDE);

  if (!check_tile_register (&dst_ref, &dst))
        abort();
}
