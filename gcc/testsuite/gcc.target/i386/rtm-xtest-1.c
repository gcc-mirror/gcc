/* { dg-do compile } */
/* { dg-options "-mrtm -dp" } */
/* { dg-final { scan-assembler "\txtest" } } */

#include <immintrin.h>

int
rtm_xtest (void)
{
  return _xtest ();
}
