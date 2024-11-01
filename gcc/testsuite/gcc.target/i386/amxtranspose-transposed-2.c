/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_transpose } */
/* { dg-options "-O2 -mamx-transpose" } */
#define AMX_TRANSPOSE
#define DO_TEST test_amx_transpose_transposed
void test_amx_transpose_transposed ();
#include "amx-helper.h"

void calc_matrix_ttransposed (__tile *dst, __tile *src)
{
  uint32_t *src_buf = (uint32_t *) src->buf;
  uint32_t *dst_buf = (uint32_t *) dst->buf;

  int M = src->rows;
  int N = src->colsb / 4;
  int i, j;

  for (i = 0; i < M; i++)
    for (j = 0; j < N; j++)
      dst_buf[j * M + i] = (uint32_t) src_buf[i * N + j];
}

void test_amx_transpose_transposed ()
{
  __tilecfg_u cfg;
  __tile src, dst, ref;

  init_tile_config (&cfg);
  init_tile_reg_and_src (1, dst);
  init_tile_reg_and_src (2, src);

  /* Check ttransposed.  */
  calc_matrix_ttransposed (&dst, &src);
  _tile_transposed (1, 2);
  _tile_stored (1, ref.buf, _STRIDE);

  if (!check_tile_register (&ref, &dst))
    abort ();
}
