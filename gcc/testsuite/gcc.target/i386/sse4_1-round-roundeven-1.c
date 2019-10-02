/* { dg-do compile } */
/* { dg-options "-O2 -msse4.1 -mfpmath=sse" } */

__attribute__((noinline, noclone)) double
f1 (double x)
{
  return __builtin_roundeven (x);
}

__attribute__((noinline, noclone)) float
f2 (float x)
{
  return __builtin_roundevenf (x);
}

/* { dg-final { scan-assembler-times "roundsd\[^\n\r\]*xmm" 1 } } */
/* { dg-final { scan-assembler-times "roundss\[^\n\r\]*xmm" 1 } } */
