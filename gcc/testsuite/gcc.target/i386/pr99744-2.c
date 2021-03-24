/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#include <x86intrin.h>

extern __m128i x, y;

__attribute__ ((target("general-regs-only")))
void
foo (void)
{
  x = _mm_move_epi64 (y); /* { dg-error "SSE register return with SSE disabled" } */
}
