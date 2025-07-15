/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_avx512 } */
/* { dg-options "-O2 -march=x86-64-v3 -mamx-avx512 -mavx512fp16" } */
#define AMX_AVX512
#define DO_TEST test_amx_avx512_movrow
void test_amx_avx512_movrow();
#include "amx-helper.h"

int j, k;
volatile __m512 cal_dst, cmp_dst;

#define TEST_MOVROW(X, Y, EI, T, INTRIN)			\
__m512								\
__attribute__((noinline, noclone, __target__("no-amx-avx512")))	\
calc_movrow##EI (__tile *src, T __A)				\
{								\
  uint8_t *src_buf = (uint8_t *)src->buf;			\
  int N = src->colsb;						\
  int vl = 512;							\
  int vl_bytes = vl >> 3;					\
  int row_index, row_chunk;					\
  __m512 res;							\
  if ((EI) == 'e')						\
  {								\
    row_index = (__A) & 0xffff;					\
    row_chunk = (((__A) >> 16) & 0xffff) * vl_bytes;		\
  }								\
  else								\
  {								\
    row_index = (__A) & 0x3f;					\
    row_chunk = ((__A) >> 6) * vl_bytes;			\
  }								\
  union512_ub tmp;						\
  for (j = 0; j < vl_bytes; j++)				\
    if (j + row_chunk >= N)					\
      tmp.u[j] = 0;						\
    else							\
      tmp.u[j] = src_buf[row_index * N + j + row_chunk];	\
  res = tmp.m;							\
  return res;							\
}								\
cal_dst = calc_movrow##EI (X, Y);				\
cmp_dst = _tile_##INTRIN (1, Y);				\
COMPARE_ZMM(cal_dst, cmp_dst);

void test_amx_avx512_movrow()
{
  __tilecfg_u cfg;
  __tile src;
  unsigned a = 2;
  char e = 'e', i = 'i';

  init_tile_config (&cfg);
  init_tile_reg_and_src (1, src);

  TEST_MOVROW (&src, a, e, unsigned, movrow);
  TEST_MOVROW (&src, 1, i, const unsigned, movrowi);

}
