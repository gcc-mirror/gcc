/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-msse2 -mfpmath=sse" } */

extern float a, b, c;

__attribute__((__target__("general-regs-only")))
void
foo (void)
{
  c = a * b; /* { dg-error "SSE register return with SSE disabled" } */
}
