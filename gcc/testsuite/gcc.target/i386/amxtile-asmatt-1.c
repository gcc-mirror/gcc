/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mamx-tile " } */
/* { dg-final { scan-assembler "ldtilecfg\[ \\t]+\(\[^\)\n\]*\)"  } } */
/* { dg-final { scan-assembler "sttilecfg\[ \\t]+\(\[^\)\n\]*\)"  } } */
/* { dg-final { scan-assembler "tilerelease"  } } */
/* { dg-final { scan-assembler "tileloadd\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tileloaddt1\[ \\t]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tilestored\[ \\t]+\[^\n\]*%tmm\[0-9\]+\[^\n\]*\\(%\[a-z0-9]*\,%\[a-z0-9\]*\,\[124\]\\)"  } } */
/* { dg-final { scan-assembler "tilezero\[ \\t]+\[^\n\]*%tmm\[0-9\]"  } } */
#include <immintrin.h>

extern int a[];
extern const void* base;
extern const int stride;

#define TMM0 0
#define TMM1 1
#define TMM2 2
#define TMM3 3

void TEST ()
{
  _tile_loadconfig (a);
  _tile_storeconfig (a);
  _tile_release ();
  _tile_loadd (TMM3, base, stride);
  _tile_stream_loadd (TMM2, base, stride);
  _tile_stored (TMM1, base, stride);
  _tile_zero (TMM0);
}
