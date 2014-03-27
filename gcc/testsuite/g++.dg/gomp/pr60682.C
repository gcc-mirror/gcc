// PR middle-end/60682
// { dg-do compile }
// { dg-options "-O2 -fopenmp-simd" }

struct A
{
  float a;
  A () {}
  A (const A &x) { a = x.a; }
};

struct B
{
  A a[16];
};

struct C
{
  float a[1];
  C () {}
  C (const C &x) { a[0] = x.a[0]; }
};

struct D
{
  C a[16];
};

void
foo (int x, B &y, D &z)
{
#pragma omp simd
  for (int i = 0; i < x; ++i)
    {
      A a;
      y.a[i] = a;
    }
#pragma omp simd
  for (int i = 0; i < x; ++i)
    {
      C a;
      z.a[i] = a;
    }
}
