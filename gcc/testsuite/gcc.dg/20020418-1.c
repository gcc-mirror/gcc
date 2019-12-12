/* PR optimization/5887 */
/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-options "-O2 -msse -ffast-math" { target i?86-*-* x86_64-*-* } } */
/* { dg-skip-if "exceeds eBPF stack limit" { bpf-*-* } } */

void bar (float *a, float *b);

void foo (char *x)
{
  float a, b;
  char c[256];
  int i, j;

  bar (&a, &b);
  for (i = 0; i < 256; i++)
    {
      float v = a;
      if (v < 0.0f) v = 0.0f;
      if (v < 255.0f) v = 255.0f;
      c[i] = v;
      a += b;
    }

  for (j = 0; j < 256; j++)
    x[j] = c[j];
}
