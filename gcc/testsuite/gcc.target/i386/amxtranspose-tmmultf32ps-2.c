/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_transpose } */
/* { dg-require-effective-target amx_tf32 } */
/* { dg-options "-O2 -mamx-transpose -mamx-tf32" } */
#define AMX_TRANSPOSE
#define AMX_TF32
#define DO_TEST test_amx_transpose_tmmultf32ps
void test_amx_transpose_tmmultf32ps();
#include "amx-helper.h"

void calc_matrix_tmmultf32ps(__tile *dst, __tile *src1, __tile *src2)
{
  float *src1_buf = (float *) src1->buf;
  float *src2_buf = (float *) src2->buf;
  float *dst_buf = (float *) dst->buf;

  int K = src1->rows;
  int M = src1->colsb / 4;
  int N = src2->colsb / 4;
  int m, n, k;

  for (m = 0; m < M; m++)
    for (k = 0; k < K; k++)
      for (n = 0; n < N; n++)
	dst_buf[m * N + n] +=
	zero_lower_mantissa_bits_fp32 (silence_snan_fp32 (src1_buf[k * M + m])) *
	zero_lower_mantissa_bits_fp32 (silence_snan_fp32 (src2_buf[k * N + n]));

}

void test_amx_transpose_tmmultf32ps ()
{
  __tilecfg_u cfg;
  __tile dst, dst_ref, src1, src2;
  uint8_t tmp_dst_buf[1024];

  init_fp32_max_tile_buffer (tmp_dst_buf);

  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, dst, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (2, src1, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (3, src2, tmp_dst_buf);

  calc_matrix_tmmultf32ps (&dst, &src1, &src2);

  _tile_tmmultf32ps (1, 2, 3);
  _tile_stored (1, dst_ref.buf, _STRIDE);

  if (!check_tile_register (&dst_ref, &dst))
    abort ();
}
