/* { dg-do compile } */
/* { dg-options "-mrtm" } */
/* { dg-final { scan-assembler "\txbegin" } } */

#include <immintrin.h>

unsigned int
rtm_test (void)
{
  return _xbegin ();
}
