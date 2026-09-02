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
/* { dg-final { scan-assembler-times "tcvtrowd2ps\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2bf16h\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2bf16l\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2phh\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "tcvtrowps2phl\[ \t]" 1 } } */
/* { dg-final { scan-assembler-times "tilemovrow\[ \t]" 2 } } */
/* { dg-final { scan-assembler-times "tilemovcol\[ \t]" 1 } } */
#include <immintrin.h>

extern int t[];
__m512i a1,a2;
__m512bh b1,b2;
__m512h c1,c2;
__m512 d;

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

void cvtrow ()
{
  d = _tile_cvtrow_epi32_ps (1, 1);
  b1 = _tile_cvtrowh_ps_pbh (2, 3);
  b2 = _tile_cvtrowl_ps_pbh (3, 5);
  c1 = _tile_cvtrowh_ps_ph (4, 7);
  c2 = _tile_cvtrowl_ps_ph (5, 9);
  a1 = _tile_extractrow (6, 2);
  _tile_insertrow (7, a1, 10);
  _tile_insertcol (2, a2, 11);
}
