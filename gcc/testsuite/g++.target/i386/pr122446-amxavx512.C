/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-avx512 -O0" } */
/* { dg-final { scan-assembler "tcvtrowd2ps\[ \\t]+%e.x,\[ \\t\]*%tmm1,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowd2ps\[ \\t]+\\\$5,\[ \\t\]*%tmm2,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2bf16h\[ \\t]+%e.x,\[ \\t\]*%tmm1,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2bf16h\[ \\t]+\\\$7,\[ \\t\]*%tmm3,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2bf16l\[ \\t]+%e.x,\[ \\t\]*%tmm2,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2bf16l\[ \\t]+\\\$3,\[ \\t\]*%tmm4,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2phh\[ \\t]+%e.x,\[ \\t\]*%tmm1,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2phh\[ \\t]+\\\$6,\[ \\t\]*%tmm2,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2phl\[ \\t]+%e.x,\[ \\t\]*%tmm3,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tcvtrowps2phl\[ \\t]+\\\$2,\[ \\t\]*%tmm4,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tilemovrow\[ \\t]+%e.x,\[ \\t\]*%tmm5,\[ \\t\]*%zmm\[0-9\]+" } } */
/* { dg-final { scan-assembler "tilemovrow\[ \\t]+\\\$4,\[ \\t\]*%tmm6,\[ \\t\]*%zmm\[0-9\]+" } } */

#include <immintrin.h>

template <int tmm_num>
struct tile_cvtrowd2ps_test
{
  __m512 operator()() { return _tile_cvtrowd2ps(tmm_num, 0); }
};

template <int tmm_num, int imm>
struct tile_cvtrowd2psi_test
{
  __m512 operator()() { return _tile_cvtrowd2psi(tmm_num, imm); }
};

template <int tmm_num>
struct tile_cvtrowps2bf16h_test
{
  __m512bh operator()() { return _tile_cvtrowps2bf16h(tmm_num, 0); }
};

template <int tmm_num, int imm>
struct tile_cvtrowps2bf16hi_test
{
  __m512bh operator()() { return _tile_cvtrowps2bf16hi(tmm_num, imm); }
};

template <int tmm_num>
struct tile_cvtrowps2bf16l_test
{
  __m512bh operator()() { return _tile_cvtrowps2bf16l(tmm_num, 0); }
};

template <int tmm_num, int imm>
struct tile_cvtrowps2bf16li_test
{
  __m512bh operator()() { return _tile_cvtrowps2bf16li(tmm_num, imm); }
};

template <int tmm_num>
struct tile_cvtrowps2phh_test
{
  __m512h operator()() { return _tile_cvtrowps2phh(tmm_num, 0); }
};

template <int tmm_num, int imm>
struct tile_cvtrowps2phhi_test
{
  __m512h operator()() { return _tile_cvtrowps2phhi(tmm_num, imm); }
};

template <int tmm_num>
struct tile_cvtrowps2phl_test
{
  __m512h operator()() { return _tile_cvtrowps2phl(tmm_num, 0); }
};

template <int tmm_num, int imm>
struct tile_cvtrowps2phli_test
{
  __m512h operator()() { return _tile_cvtrowps2phli(tmm_num, imm); }
};

template <int tmm_num>
struct tile_movrow_test
{
  __m512 operator()() { return _tile_movrow(tmm_num, 0); }
};

template <int tmm_num, int imm>
struct tile_movrowi_test
{
  __m512 operator()() { return _tile_movrowi(tmm_num, imm); }
};

void test_amx_avx512()
{
  __m512 r1 = tile_cvtrowd2ps_test<1>()();
  __m512 r2 = tile_cvtrowd2psi_test<2, 5>()();
  __m512bh r3 = tile_cvtrowps2bf16h_test<1>()();
  __m512bh r4 = tile_cvtrowps2bf16hi_test<3, 7>()();
  __m512bh r5 = tile_cvtrowps2bf16l_test<2>()();
  __m512bh r6 = tile_cvtrowps2bf16li_test<4, 3>()();
  __m512h r7 = tile_cvtrowps2phh_test<1>()();
  __m512h r8 = tile_cvtrowps2phhi_test<2, 6>()();
  __m512h r9 = tile_cvtrowps2phl_test<3>()();
  __m512h r10 = tile_cvtrowps2phli_test<4, 2>()();
  __m512 r11 = tile_movrow_test<5>()();
  __m512 r12 = tile_movrowi_test<6, 4>()();
}
