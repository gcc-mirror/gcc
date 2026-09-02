/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -macev1" } */
#include <immintrin.h>

void ace ()
{
  _tile_ace_zero (8); /* { dg-error "the tmm register number argument must be between 0 to 7" "" { target { ! ia32 } } 0 } */
}
