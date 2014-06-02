/* Test that pause instruction works even when SSE is not enabled.  */
/* { dg-do compile } */
/* { dg-options "-O2 -dp" } */
/* { dg-final { scan-assembler-times "\\*pause" 1 } } */

#include <xmmintrin.h>

void
foo (void)
{
  _mm_pause ();
}
