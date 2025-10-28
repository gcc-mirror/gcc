/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-int8 -O0" } */
/* { dg-final { scan-assembler "tdpbssd\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm0" } } */
/* { dg-final { scan-assembler "tdpbsud\[ \\t]+%tmm5,\[ \\t\]*%tmm4,\[ \\t\]*%tmm3" } } */
/* { dg-final { scan-assembler "tdpbusd\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm6" } } */
/* { dg-final { scan-assembler "tdpbuud\[ \\t]+%tmm1,\[ \\t\]*%tmm0,\[ \\t\]*%tmm7" } } */

#include <immintrin.h>

template <int dst, int src1, int src2>
struct dpbssd
{
  void operator()() { _tile_dpbssd(dst, src1, src2); }
};

template <int dst, int src1, int src2>
struct dpbsud
{
  void operator()() { _tile_dpbsud(dst, src1, src2); }
};

template <int dst, int src1, int src2>
struct dpbusd
{
  void operator()() { _tile_dpbusd(dst, src1, src2); }
};

template <int dst, int src1, int src2>
struct dpbuud
{
  void operator()() { _tile_dpbuud(dst, src1, src2); }
};

void test_amx_int8()
{
  dpbssd<0, 1, 2>()();
  dpbsud<3, 4, 5>()();
  dpbusd<6, 1, 2>()();
  dpbuud<7, 0, 1>()();
}
