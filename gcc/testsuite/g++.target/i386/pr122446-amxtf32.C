/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-tf32 -O0" } */
/* { dg-final { scan-assembler "tmmultf32ps\[ \\t]+%tmm2,\[ \\t\]*%tmm1,\[ \\t\]*%tmm0" } } */

#include <immintrin.h>

template <int dst, int src1, int src2>
struct mmultf32ps
{
  void operator()() { _tile_mmultf32ps(dst, src1, src2); }
};

void test_amx_tf32()
{
  mmultf32ps<0, 1, 2>()();
}
