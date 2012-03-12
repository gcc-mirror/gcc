/* { dg-do compile } */
/* { dg-options "-mrtm" } */
/* { dg-final { scan-assembler "\txabort" } } */

#include <immintrin.h>

void
rtm_test (void)
{
  _xabort (13);
}
