/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -march=x86-64-v3 -mamx-avx512 -masm=intel" } */
/* { dg-final { scan-assembler-times "tcvtrowd2ps\[ \\t]+\[^\n\]*zmm\[0-9\]+\[^\n\]*tmm1+\[^\n\]*" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2pbf16h\[ \\t]+\[^\n\]*zmm\[0-9\]+\[^\n\]*tmm1+\[^\n\]*" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2pbf16l\[ \\t]+\[^\n\]*zmm\[0-9\]+\[^\n\]*tmm1+\[^\n\]*" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2phh\[ \\t]+\[^\n\]*zmm\[0-9\]+\[^\n\]*tmm1+\[^\n\]*" 2 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2phl\[ \\t]+\[^\n\]*zmm\[0-9\]+\[^\n\]*tmm1+\[^\n\]*" 2 } } */
/* { dg-final { scan-assembler-times "tilemovrow\[ \\t]+\[^\n\]*zmm\[0-9\]+\[^\n\]*tmm1+\[^\n\]*" 2 } } */
#include <immintrin.h>

__m512 a;
__m512bh b;
__m512h c;

void TEST ()
{
  a = _tile_cvtrowd2ps (1, 1);
  a = _tile_cvtrowd2psi (1, 2);
  b = _tile_cvtrowps2pbf16h (1, 3);
  b = _tile_cvtrowps2pbf16hi (1, 4);
  b = _tile_cvtrowps2pbf16l (1, 5);
  b = _tile_cvtrowps2pbf16li (1, 6);
  c = _tile_cvtrowps2phh (1, 7);
  c = _tile_cvtrowps2phhi (1, 8);
  c = _tile_cvtrowps2phl (1, 9);
  c = _tile_cvtrowps2phli (1, 10);
  a = _tile_movrow (1, 11);
  a = _tile_movrowi (1, 12);
}
