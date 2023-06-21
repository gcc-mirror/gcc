/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_complex } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-options "-O2 -mamx-complex -mavx512fp16" } */
#define AMX_COMPLEX
#define DO_TEST test_amx_complex_cmmimfp16ps
void test_amx_complex_cmmimfp16ps ();
#include "amx-helper.h"

void calc_matrix_cmmimfp16ps (__tile *dst, __tile *src1, __tile *src2)
{
  uint16_t *src1_buf = (uint16_t *) src1->buf;
  uint16_t *src2_buf = (uint16_t *) src2->buf;
  float *dst_buf = (float *) dst->buf;
  
  int M = src1->rows;
  int N = src1->colsb / 4;
  int K = src2->colsb / 4;
  int i, j, k, t;

  for (i = 0; i < M; i++)
    for (j = 0; j < N; j++)
      for (k = 0; k < K; k++)
	for (t = 0; t < 2; t+=2)
	  dst_buf[i * N + k] +=
	  (make_fp16_f32(src1_buf[i * 2 * N + 2 * j + t]) *
	    make_fp16_f32(src2_buf[j * 2 * K + 2 * k + t + 1])) +
	  (make_fp16_f32(src1_buf[i * 2 * N + 2 * j + t + 1]) *
	    make_fp16_f32(src2_buf[j * 2 * K + 2 * k + t]));
}

void test_amx_complex_cmmimfp16ps ()
{
  __tilecfg_u cfg;
  __tile dst, dst_ref, src1, src2;
  uint8_t tmp_dst_buf[1024], tmp_dst_zero_buf[1024];

  init_fp16_max_tile_buffer (tmp_dst_buf);
  init_fp16_max_tile_zero_buffer (tmp_dst_zero_buf);

  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, dst, tmp_dst_zero_buf);
  init_tile_reg_and_src_with_buffer (2, src1, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (3, src2, tmp_dst_buf);

  calc_matrix_cmmimfp16ps (&dst, &src1, &src2);
  
  _tile_cmmimfp16ps (1, 2, 3);
  _tile_stored (1, dst_ref.buf, _STRIDE);

  if (!check_tile_register (&dst_ref, &dst))
        abort ();
}
