/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64-v3 -mamx-avx512" } */
/* { dg-final { scan-assembler-times "tcvtrowd2ps\[ \\t]" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2pbf16h\[ \\t]" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2pbf16l\[ \\t]" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2phh\[ \\t]" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2phl\[ \\t]" 2 } } */
/* { dg-final { scan-assembler-times "tilemovrow\[ \\t]" 2 } } */
#include <immintrin.h>

#define TMM1 1

__m512 a;
__m512bh b;
__m512h c;

void TEST ()
{
  a = _tile_cvtrowd2ps (TMM1, 1);
  a = _tile_cvtrowd2psi (TMM1, 2);
  b = _tile_cvtrowps2pbf16h (TMM1, 3);
  b = _tile_cvtrowps2pbf16hi (TMM1, 4);
  b = _tile_cvtrowps2pbf16l (TMM1, 5);
  b = _tile_cvtrowps2pbf16li (TMM1, 6);
  c = _tile_cvtrowps2phh (TMM1, 7);
  c = _tile_cvtrowps2phhi (TMM1, 8);
  c = _tile_cvtrowps2phl (TMM1, 9);
  c = _tile_cvtrowps2phli (TMM1, 10);
  a = _tile_movrow (TMM1, 11);
  a = _tile_movrowi (TMM1, 12);
}
