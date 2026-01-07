/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mamx-tile -mamx-int8 -O0" } */
/* { dg-final { scan-assembler "tdpbssd\[ \\t]+\[^\n\]*%tmm2+\[^\n\]*%tmm1+\[^\n\]*%tmm0" } } */

#include <immintrin.h>

template <int hello, int crazy, int gcc>
struct dpbssd
{
  void operator()() { _tile_dpbssd(hello, crazy, gcc); }
};

void f()
{
  dpbssd<0, 1, 2>()();
}

