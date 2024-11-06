/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_avx512 } */
/* { dg-options "-O2 -march=x86-64-v3 -mamx-avx512" } */
#define AMX_AVX512
#define DO_TEST test_amx_avx512_cvtrowd2ps
void test_amx_avx512_cvtrowd2ps();
#include "amx-helper.h"

volatile __m512 cal_dst, cmp_dst;

#define DEFINE_TEST_CVTROWD2PS(EI, T)				\
__m512								\
__attribute__((noinline, noclone, __target__("no-amx-avx512")))	\
calc_cvtrowd2ps##EI (__tile *src, T __A)			\
{								\
  uint32_t *src_buf = (uint32_t *)src->buf;			\
  int N = src->colsb / 4;					\
  int vl = 512;							\
  int vl_bytes = vl >> 3;					\
  int row_index, row_chunk, j;					\
  __m512 res;							\
  if ((#EI) == "e")						\
  {								\
    row_index = (__A) & 0xffff;					\
    row_chunk = (((__A) >> 16) & 0xffff) * vl_bytes;		\
  }								\
  else								\
  {								\
    row_index = (__A) & 0x3f;					\
    row_chunk = ((__A) >> 6) * vl_bytes;			\
  }								\
  for (j = 0; j < vl_bytes / 4; j++)				\
    if (j + row_chunk / 4 >= N)					\
      res[j] = 0;						\
    else							\
      res[j] = (float) (int) src_buf[row_index * N + j + row_chunk / 4];	\
  return res;							\
}

DEFINE_TEST_CVTROWD2PS(e, unsigned)
DEFINE_TEST_CVTROWD2PS(i, const unsigned)

#define TEST_CVTROWD2PS(X, Y, EI, T, INTRIN)			\
cal_dst = calc_cvtrowd2ps##EI (X, Y);				\
cmp_dst = _tile_##INTRIN (1, Y);				\
COMPARE_ZMM(cal_dst, cmp_dst);

void test_amx_avx512_cvtrowd2ps()
{
  __tilecfg_u cfg;
  __tile src;
  uint8_t tmp_dst_buf[1024];
  unsigned a = 2;

  init_int32_max_tile_buffer (tmp_dst_buf);

  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, src, tmp_dst_buf);

  TEST_CVTROWD2PS (&src, a, e, unsigned, cvtrowd2ps);
  TEST_CVTROWD2PS (&src, 1, i, const unsigned, cvtrowd2psi);
}
