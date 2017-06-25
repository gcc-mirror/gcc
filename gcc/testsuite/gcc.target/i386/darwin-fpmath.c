/* { dg-do compile { target i?86-*-darwin* x86_64-*-darwin* } } */
/* { dg-final { scan-assembler "addsd" } } */
/* Do not add -msse or -msse2 or -mfpmath=sse to the options.  GCC is
   supposed to use SSE math on Darwin by default, and libm won't work
   right if it doesn't.  */
double foo(double x, double y)
{
  return x + y;
}
