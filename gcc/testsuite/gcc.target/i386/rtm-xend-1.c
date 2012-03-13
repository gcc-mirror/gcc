/* { dg-do compile } */
/* { dg-options "-mrtm" } */
/* { dg-final { scan-assembler "\txend" } } */

#include <immintrin.h>

void
rtm_test (void)
{
  _xend ();
}
