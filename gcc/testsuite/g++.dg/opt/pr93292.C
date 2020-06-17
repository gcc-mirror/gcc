// PR tree-optimization/93292
// { dg-do compile }
// { dg-options "-O3 -w" }

struct A {
  static int foo (float x) { static int b; b = x ? x + 0.5 : 0; return b; }
};

void
bar (int *d, float e)
{
  float g;
  for (int h = 0; h < 64; h++)
    {
      d[h] += A::foo (g < 0 ? : g > 5 ? : g);
      A::foo (e);
    }
}
