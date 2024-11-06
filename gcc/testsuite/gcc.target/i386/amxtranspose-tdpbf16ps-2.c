/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_transpose } */
/* { dg-require-effective-target amx_bf16 } */
/* { dg-options "-O2 -mamx-transpose -mamx-bf16 -mavx512bf16" } */
#define AMX_TRANSPOSE
#define AMX_BF16
#define DO_TEST test_amx_transpose_tdpbf16ps
void test_amx_transpose_tdpbf16ps ();
#include "amx-helper.h"

void calc_matrix_tdpbf16ps(__tile *dst, __tile *src1, __tile *src2)
{
  uint16_t *src1_buf = (uint16_t *) src1->buf;
  uint16_t *src2_buf = (uint16_t *) src2->buf;
  float *dst_buf = (float *) dst->buf;

  int K = src1->rows;
  int M = src1->colsb / 4;
  int N = src2->colsb / 4;
  int m, k, n, t;

  for (m = 0; m < M; m++)
    for (k = 0; k < K; k++)
      for (n = 0; n < N; n++)
	for (t = 0; t < 2; t+=2)
	  dst_buf[m * N + n] +=
	    (make_bf16_f32 (src1_buf[k * 2 * M + 2 * m + t]) *
	      make_bf16_f32 (src2_buf[k * 2 * N + 2 * n + t])) +
	    (make_bf16_f32 (src1_buf[k * 2 * M + 2 * m + t + 1]) *
	      make_bf16_f32 (src2_buf[k * 2 * N + 2 * n + t + 1]));
}

void test_amx_transpose_tdpbf16ps ()
{
  __tilecfg_u cfg;
  __tile dst, dst_ref, src1, src2;
  uint8_t tmp_dst_buf[1024];

  init_bf16_max_tile_buffer (tmp_dst_buf);

  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, dst, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (2, src1, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (3, src2, tmp_dst_buf);

  calc_matrix_tdpbf16ps (&dst, &src1, &src2);

  _tile_tdpbf16ps (1, 2, 3);
  _tile_stored (1, dst_ref.buf, _STRIDE);

  if (!check_float_tile_register (&dst_ref, &dst))
    abort ();
}
