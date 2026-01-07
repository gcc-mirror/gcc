/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-fp16 -O0" } */
/* { dg-final { scan-assembler "tdpfp16ps\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm0" } } */

#include <immintrin.h>

template <int dst, int src1, int src2>
struct dpfp16ps
{
  void operator()() { _tile_dpfp16ps(dst, src1, src2); }
};

void test_amx_fp16()
{
  dpfp16ps<0, 1, 2>()();
}
