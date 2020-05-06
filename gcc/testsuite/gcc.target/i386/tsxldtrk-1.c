/* { dg-do compile } */
/* { dg-options "-O2 -mtsxldtrk" } */
/* { dg-final { scan-assembler "xsusldtrk" } } */
/* { dg-final { scan-assembler "xresldtrk" } } */

#include <immintrin.h>

void
foo (void)
{
  _xsusldtrk ();
  _xresldtrk ();
}
