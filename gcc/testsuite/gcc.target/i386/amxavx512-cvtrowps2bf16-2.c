/* { dg-do run { target { ! ia32 } } } */
/* { dg-require-effective-target amx_avx512 } */
/* { dg-options "-O2 -march=x86-64-v3 -mamx-avx512" } */
#define AMX_AVX512
#define DO_TEST test_amx_avx512_cvtrowps2bf16
void test_amx_avx512_cvtrowps2bf16();
#include "amx-helper.h"

volatile __m512bh cal_dst, cmp_dst;

#define DEFINE_TEST_CVTROWPS2BF16(HL, EI, T)			\
__m512bh							\
__attribute__((noinline, noclone, __target__("no-amx-avx512")))	\
calc_cvtrowps2bf16##HL##EI (__tile *src, T __A)		\
{								\
  float *src_buf = (float *) src->buf;				\
  int N = src->colsb / 4;					\
  int vl = 512;							\
  int vl_bytes = vl >> 3;					\
  int row_index, row_chunk, zeropos, pos, j, k;			\
  __m512bh res;							\
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
  if ((#HL) == "h")						\
  {								\
    zeropos = 0;						\
    pos = 1;							\
  }								\
  else								\
  {								\
    zeropos = 1;						\
    pos = 0;							\
  }								\
  for (j = 0; j < vl_bytes / 4; j++)				\
    if (j + row_chunk / 4 >= N)					\
      for (k = 0; k < 2; k++)					\
	res[2 * j + k] = 0;					\
    else							\
    {								\
      union16bh_uw tmp;						\
      tmp.u = make_f32_bf16 (src_buf[row_index * N + j + row_chunk / 4]);	\
      res[2 * j + pos] = tmp.bf16;				\
      res[2 * j + zeropos] = (__bf16) 0;			\
    }								\
  return res;							\
}

DEFINE_TEST_CVTROWPS2BF16(h, e, unsigned)
DEFINE_TEST_CVTROWPS2BF16(l, e, unsigned)
DEFINE_TEST_CVTROWPS2BF16(h, i, const unsigned)
DEFINE_TEST_CVTROWPS2BF16(l, i, const unsigned)

#define TEST_CVTROWPS2BF16(X, Y, HL, EI, T, INTRIN)		\
cal_dst = calc_cvtrowps2bf16##HL##EI (X, Y);			\
cmp_dst = _tile_##INTRIN (1, Y);				\
COMPARE_ZMM_BF16(cal_dst, cmp_dst);

void test_amx_avx512_cvtrowps2bf16 ()
{
  __tilecfg_u cfg;
  __tile src;
  uint8_t tmp_dst_buf[1024];
  unsigned a = 2;

  init_fp32_max_tile_buffer (tmp_dst_buf);

  init_tile_config (&cfg);
  init_tile_reg_and_src_with_buffer (1, src, tmp_dst_buf);

  TEST_CVTROWPS2BF16 (&src, a, h, e, unsigned, cvtrowps2bf16h);
  TEST_CVTROWPS2BF16 (&src, a, l, e, unsigned, cvtrowps2bf16l);
  TEST_CVTROWPS2BF16 (&src, 1, h, i, const unsigned, cvtrowps2bf16hi);
  TEST_CVTROWPS2BF16 (&src, 1, l, i, const unsigned, cvtrowps2bf16li);
}
