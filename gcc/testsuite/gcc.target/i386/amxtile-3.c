/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mamx-tile " } */
/* { dg-final { scan-assembler "tileloadd\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tileloaddt1\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tilestored\[ \\t]+\[^\n\]*%tmm\[0-9\]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)"  } } */
/* { dg-final { scan-assembler "leaq\[ \\t]+4" } } */
/* { dg-final { scan-assembler "leaq\[ \\t]+8" } } */
/* { dg-final { scan-assembler "addq\[ \\t]+\\\$12" } } */
/* { dg-final { scan-assembler-not "leaq\[ \\t]+1" } } */
/* { dg-final { scan-assembler-not "leaq\[ \\t]+2" } } */
/* { dg-final { scan-assembler-not "addq\[ \\t]+\\\$3" } } */
#include <immintrin.h>

extern int a[];
extern const float* base;
extern const int stride;

#define TMM0 0
#define TMM1 1
#define TMM2 2
#define TMM3 3

void TEST ()
{
  _tile_loadd (TMM3, base + 1, stride);
  _tile_stream_loadd (TMM2, base + 2, stride);
  _tile_stored (TMM2, base + 3, stride);
}
