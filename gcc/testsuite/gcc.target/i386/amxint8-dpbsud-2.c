/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_tile } */
/* { dg-require-effective-target amx_int8 } */
/* { dg-options "-O2 -mamx-tile -mamx-int8" } */
#include <immintrin.h>

#define AMX_INT8
#define DO_TEST test_amx_int8_dpbsud
void test_amx_int8_dpbsud ();
#include "amx-check.h"

/* Init tile buffer with int32 value*/
void init_i32_max_tile_buffer (uint8_t *buf)
{
  int i, j;
  int *ptr = (int *)buf;
  for (i = 0; i < 16; i++)
    for (j = 0; j < 16; j++)
      ptr[i * 16 + j] = 2 * i - (16 - j);
}

void calc_matrix_dpbsud (__tile *dst, __tile *src1, __tile *src2)
{
  int8_t *src1_buf = (int8_t *)src1->buf;
  uint8_t *src2_buf = (uint8_t *)src2->buf;
  int *dst_buf = (int *)dst->buf;

  int M = src1->rows;
  int N = src1->colsb / 4;
  int K = src2->colsb / 4;
  int i, j, k, t;

  for (i = 0; i < M; i++)
    for (j = 0; j < N; j++)
      for (k = 0; k < K; k++)
	for (t = 0; t < 4; t++)
	  {
	    dst_buf[i * N + k] += 
	      ((int) src1_buf[i * 4 * N + 4 * j + t]) *
	      ((unsigned) src2_buf[j * 4 * K + 4 * k + t]);
	  }
}

void test_amx_int8_dpbsud ()
{
  __tilecfg_u cfg;
  __tile dst, dst_ref, src1, src2;
  uint8_t tmp_dst_buf[1024];
  
  init_i32_max_tile_buffer (tmp_dst_buf);

  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, dst, tmp_dst_buf);
  init_tile_reg_and_src (2, src1);
  init_tile_reg_and_src (3, src2);

  calc_matrix_dpbsud (&dst, &src1, &src2);
  _tile_dpbsud (1, 2, 3);
  _tile_stored (1, dst_ref.buf, _STRIDE);
  
  if (!check_tile_register (&dst_ref, &dst))
      abort();
}
