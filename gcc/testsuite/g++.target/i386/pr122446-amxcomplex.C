/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-complex -O0" } */
/* { dg-final { scan-assembler "tcmmimfp16ps\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm0" } } */
/* { dg-final { scan-assembler "tcmmrlfp16ps\[ \\t]+%tmm5,\[ \\t\]*%tmm4,\[ \\t\]*%tmm3" } } */

#include <immintrin.h>

template <int dst, int src1, int src2>
struct cmmimfp16ps
{
  void operator()() { _tile_cmmimfp16ps(dst, src1, src2); }
};

template <int dst, int src1, int src2>
struct cmmrlfp16ps
{
  void operator()() { _tile_cmmrlfp16ps(dst, src1, src2); }
};

void test_amx_complex()
{
  cmmimfp16ps<0, 1, 2>()();
  cmmrlfp16ps<3, 4, 5>()();
}
