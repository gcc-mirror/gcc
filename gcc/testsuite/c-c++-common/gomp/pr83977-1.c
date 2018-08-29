/* PR middle-end/83977 */
/* { dg-do compile } */
/* { dg-additional-options "-O2 -w" } */

struct S { int a, b, c; };

#pragma omp declare simd uniform(z) linear(v:1)
__attribute__((noinline)) static int
foo (int x, int y, struct S z, int u, int v)
{
  return x + y + z.a;
}

int
bar (int x, int y, int z)
{
  struct S s = { z, 1, 1 };
  return foo (x, y, s, 0, 0);
}
