/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3 -mfpmath=sse" } */
/* { dg-final { scan-assembler-not "vblendv" } } */

void
foo (float* a, float* b, float* c, float* __restrict d, int n)
{
  for (int i = 0; i != n; i++)
    {
      c[i] *= 2.0f;
      if (a[i] > b[i])
        d[i] = 0.0f;
      else
        d[i] = c[i];
    }
}
