/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-movrs -O0" } */
/* { dg-final { scan-assembler "tileloaddrs\[ \\t]+\[^\n\]*,\[ \\t\]*%tmm0" } } */
/* { dg-final { scan-assembler "tileloaddrst1\[ \\t]+\[^\n\]*,\[ \\t\]*%tmm1" } } */

#include <immintrin.h>

template <int tmm_num>
struct tile_loaddrs_test
{
  void operator()(const void* base, int stride)
  {
    _tile_loaddrs(tmm_num, base, stride);
  }
};

template <int tmm_num>
struct tile_loaddrst1_test
{
  void operator()(const void* base, int stride)
  {
    _tile_loaddrst1(tmm_num, base, stride);
  }
};

void test_amx_movrs()
{
  char buf[1024];
  tile_loaddrs_test<0>()(buf, 64);
  tile_loaddrst1_test<1>()(buf, 64);
}
