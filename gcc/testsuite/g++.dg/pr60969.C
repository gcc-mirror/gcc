/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-options "-O2 -ftree-vectorize -march=pentium4 -mfpmath=387" } */

struct A
{
  float f, g, h, k;
  A () {}
  A (float v0, float x, float y) : f(v0), g(x), h(y), k(0.0f) {}
  A bar (A &a, float t) { return A (f + a.f * t, g + a.g * t, h + a.h * t); }
};

A
baz (A &x, A &y, float t)
{
  return x.bar (y, t);
}

A *
foo (A &s, A &t, A &u, A &v, int y, int z)
{
  A *x = new A[y * z];
  for (int i = 0; i < 7; i++)
    {
      A s = baz (s, u, i / (float) z);
      A t = baz (t, v, i / (float) z);
      for (int j = 0; j < 7; j++)
        x[i * y + j] = baz (s, t, j / (float) y);
    }
  return x;
}

/* { dg-final { scan-assembler-not "%mm" } } */
