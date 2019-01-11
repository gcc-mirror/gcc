/* { dg-do compile } */
/* { dg-options "-Os -msse -mno-sse3 -mfpmath=387" } */

int test (float a)
{
  return (a * a);
}

/* { dg-final { scan-assembler-not "cvttss2si\[lq\]?\[^\\n\]*%xmm" } } */
