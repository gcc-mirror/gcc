/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_transpose } */
/* { dg-require-effective-target amx_complex } */
/* { dg-require-effective-target avx512fp16 } */
/* { dg-options "-O2 -mamx-transpose -mamx-complex -mavx512fp16" } */
#define AMX_TRANSPOSE
#define AMX_COMPLEX
#define DO_TEST test_amx_transpose_conjtfp16
void test_amx_transpose_conjtfp16 ();
#include "amx-helper.h"

void calc_matrix_conjtfp16 (__tile *dst, __tile *src)
{
  uint16_t *src_buf = (uint16_t *) src->buf;
  float *dst_buf = (float *) dst->buf;

  int M = dst->rows;
  int N = dst->colsb / 4;
  int i, j, t;

  for (i = 0; i < M; i++)
    for (j = 0; j < N; j++)
      for (t = 0; t < 2; t+=2)
      {
	dst_buf[i * 2 * N + 2 * j + t] = src_buf[j * 2 * M + 2 * i + t];
	dst_buf[i * 2 * N + 2 * j + t + 1] = -src_buf[j * 2 * M + 2 * i + t + 1];
      }
}

void test_amx_transpose_conjtfp16 ()
{
  __tilecfg_u cfg;
  __tile src, dst, ref;
  uint8_t tmp_dst_buf[1024];

  init_fp16_max_tile_buffer (tmp_dst_buf);
  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, dst, tmp_dst_buf);
  init_tile_reg_and_src_with_buffer (2, src, tmp_dst_buf);

  /* Check tconjtfp16.  */
  calc_matrix_conjtfp16 (&dst, &src);
  _tile_conjtfp16 (1, 2);
  _tile_stored (1, ref.buf, _STRIDE);

  if (!check_tile_register (&ref, &dst))
    abort ();
}
