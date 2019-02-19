/* { dg-do compile } */
/* { dg-options "-mfpmath=sse,387 -msoft-float -mno-sse" } */

_Atomic double a;
int b;

void
foo (void)
{
  a += b; /* { dg-error "SSE register return with SSE disabled" "" { target { ! ia32 } } } */
}
