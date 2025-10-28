/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -O0" } */
/* { dg-final { scan-assembler "tileloadd\[ \\t]+\[^\n\]*,\[ \\t\]*%tmm0" } } */
/* { dg-final { scan-assembler "tilestored\[ \\t]+%tmm1," } } */
/* { dg-final { scan-assembler "tilezero\[ \\t]+%tmm3" } } */
/* { dg-final { scan-assembler "tileloaddt1\[ \\t]+\[^\n\]*,\[ \\t\]*%tmm2" } } */

#include <immintrin.h>

template <int tmm_num>
struct tile_loadd_test
{
  void operator()(const void* base, int stride)
  {
    _tile_loadd(tmm_num, base, stride);
  }
};

template <int tmm_num>
struct tile_stored_test
{
  void operator()(void* base, int stride)
  {
    _tile_stored(tmm_num, base, stride);
  }
};

template <int tmm_num>
struct tile_zero_test
{
  void operator()() { _tile_zero(tmm_num); }
};

template <int tmm_num>
struct tile_stream_loadd_test
{
  void operator()(const void* base, int stride)
  {
    _tile_stream_loadd(tmm_num, base, stride);
  }
};

void test_amx_tile()
{
  char buf[1024];
  tile_loadd_test<0>()(buf, 64);
  tile_stored_test<1>()(buf, 64);
  tile_stream_loadd_test<2>()(buf, 64);
  tile_zero_test<3>()();
}
