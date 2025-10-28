/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-fp8 -O0" } */
/* { dg-final { scan-assembler "tdpbf8ps\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm0" } } */
/* { dg-final { scan-assembler "tdpbhf8ps\[ \\t]+%tmm5,\[ \\t\]*%tmm4,\[ \\t\]*%tmm3" } } */
/* { dg-final { scan-assembler "tdphbf8ps\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm6" } } */
/* { dg-final { scan-assembler "tdphf8ps\[ \\t]+%tmm1,\[ \\t\]*%tmm0,\[ \\t\]*%tmm7" } } */

#include <immintrin.h>

template <int dst, int src1, int src2>
struct dpbf8ps
{
  void operator()() { _tile_dpbf8ps(dst, src1, src2); }
};

template <int dst, int src1, int src2>
struct dpbhf8ps
{
  void operator()() { _tile_dpbhf8ps(dst, src1, src2); }
};

template <int dst, int src1, int src2>
struct dphbf8ps
{
  void operator()() { _tile_dphbf8ps(dst, src1, src2); }
};

template <int dst, int src1, int src2>
struct dphf8ps
{
  void operator()() { _tile_dphf8ps(dst, src1, src2); }
};

void test_amx_fp8()
{
  dpbf8ps<0, 1, 2>()();
  dpbhf8ps<3, 4, 5>()();
  dphbf8ps<6, 1, 2>()();
  dphf8ps<7, 0, 1>()();
}
