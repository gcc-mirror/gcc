/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -macev1" } */
/* { dg-final { scan-assembler-times "ldtilecfg\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "sttilecfg\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "tilerelease" 1 } } */
/* { dg-final { scan-assembler-times "tilezero\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "bsrinit\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "bsrmovf\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "bsrmovl\[ \t]" 2 } } */
/* { dg-final { scan-assembler-times "bsrmovh\[ \t]" 2 } } */
#include <immintrin.h>

extern int t[];
__m512i a1,a2;

void amxtile ()
{
  _tile_ace_loadconfig (t);
  _tile_ace_storeconfig (t);
  _tile_ace_release ();
  _tile_ace_zero (0);
}

void bsr ()
{
  _bsr0_init ();
  _bsr0_insertfull (a1, a2);
  _bsr0_inserth (a1);
  a1 = _bsr0_extracth ();
  _bsr0_insertl (a2);
  a2 = _bsr0_extractl ();
}
