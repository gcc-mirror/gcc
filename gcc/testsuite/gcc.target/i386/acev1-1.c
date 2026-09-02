/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -macev1" } */
/* { dg-final { scan-assembler-times "ldtilecfg\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "sttilecfg\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "tilerelease" 1 } } */
/* { dg-final { scan-assembler-times "tilezero\[ \t]" 1 } } */
#include <immintrin.h>

extern int t[];

void amxtile ()
{
  _tile_ace_loadconfig (t);
  _tile_ace_storeconfig (t);
  _tile_ace_release ();
  _tile_ace_zero (0);
}
