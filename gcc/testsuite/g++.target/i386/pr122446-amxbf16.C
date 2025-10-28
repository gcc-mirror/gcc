/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-bf16 -O0" } */
/* { dg-final { scan-assembler "tdpbf16ps\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm0" } } */

#include <immintrin.h>

template <int dst, int src1, int src2>
struct dpbf16ps
{
  void operator()() { _tile_dpbf16ps(dst, src1, src2); }
};

void test_amx_bf16()
{
  dpbf16ps<0, 1, 2>()();
}
