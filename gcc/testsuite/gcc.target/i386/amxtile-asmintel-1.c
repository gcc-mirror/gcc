/* { dg-do compile { target { ! ia32 } } } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-O2 -mamx-tile -masm=intel " } */
/* { dg-final { scan-assembler "ldtilecfg\[ \\t]"  } } */
/* { dg-final { scan-assembler "sttilecfg\[ \\t]"  } } */
/* { dg-final { scan-assembler "tilerelease"  } } */
/* { dg-final { scan-assembler "tileloadd\[ \\t]%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tileloaddt1\[ \\t]%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tilestored\[ \\t]\[^\n\]+\[^\n\]*%tmm\[0-9\]"  } } */
/* { dg-final { scan-assembler "tilezero\[ \\t]+\[^\n\]*%tmm\[0-9\]"  } } */
#include <immintrin.h>

extern int a[];
extern const void* base;
extern const int stride;
void TEST ()
{
  _tile_loadconfig (a);
  _tile_storeconfig (a);
  _tile_release ();
  _tile_loadd (5, base, stride);
  _tile_stream_loadd (4, base, stride);
  _tile_stored (3, base, stride);
  _tile_zero (2);
}
