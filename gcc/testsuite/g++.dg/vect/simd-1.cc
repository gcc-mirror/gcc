// { dg-require-effective-target vect_simd_clones }
// { dg-additional-options "-fopenmp-simd" }
// { dg-additional-options "-mavx" { target avx_runtime } }

#include "../../gcc.dg/vect/tree-vect.h"

int w;
struct S {
  int s, &t;
  int *p;
  S (int *x) : s (0), t (w), p (x) {};
  void foo (short &, int &);
  void bar (short &, int &);
  void baz (short &, int &);
  void qux (short &, int &);
};

__attribute__((noipa)) void
S::foo (short &x, int &y)
{
  int *q = this->p;
  #pragma omp simd lastprivate (x, s, t) private (y)
  for (int i = 0; i < 1025; ++i)
    {
      y = q[i];
      x = y;
      q[i] = y * 2;
      s = q[i] + 3;
      t = q[i] + 6;
    }
}

__attribute__((noipa)) void
S::bar (short &x, int &y)
{
  #pragma omp simd linear (x) linear (s, t: 2) private (y)
  for (int i = 0; i < 1025; ++i)
    {
      y = p[i];
      x += y - 2 * i + 1;
      p[i] = y * 2;
      s += 2 * y - 4 * i + 2;
      t += 2 * y - 4 * i + 2;
    }
}

__attribute__((noipa)) void
S::baz (short &x, int &y)
{
  int *q = this->p;
  #pragma omp simd lastprivate (x, s, t) private (y) if (simd: 0)
  for (int i = 0; i < 1025; ++i)
    {
      y = q[i];
      x = y;
      q[i] = y * 2;
      s = q[i] + 3;
      t = q[i] + 6;
    }
}

__attribute__((noipa)) void
S::qux (short &x, int &y)
{
  #pragma omp simd linear (x) linear (s, t: 2) private (y) simdlen (1)
  for (int i = 0; i < 1025; ++i)
    {
      y = p[i];
      x += y - 2 * i + 1;
      p[i] = y * 2;
      s += 2 * y - 4 * i + 2;
      t += 2 * y - 4 * i + 2;
    }
}

int
main ()
{
  short x;
  int a[1025], y;
  check_vect ();
  S s = a;
  for (int i = 0; i < 1025; ++i)
    {
      a[i] = i;
      asm volatile ("" : "+g" (i));
    }
  s.foo (x, y);
  if (x != 1024 || s.s != 2051 || s.t != 2054)
    abort ();
  for (int i = 0; i < 1025; ++i)
    if (a[i] != 2 * i)
      abort ();
  s.bar (x, y);
  if (x != 2049 || s.s != 4101 || s.t != 4104)
    abort ();
  for (int i = 0; i < 1025; ++i)
    if (a[i] != 4 * i)
      abort ();
    else
      a[i] = i;
  s.baz (x, y);
  if (x != 1024 || s.s != 2051 || s.t != 2054)
    abort ();
  for (int i = 0; i < 1025; ++i)
    if (a[i] != 2 * i)
      abort ();
  s.qux (x, y);
  if (x != 2049 || s.s != 4101 || s.t != 4104)
    abort ();
  for (int i = 0; i < 1025; ++i)
    if (a[i] != 4 * i)
      abort ();
}
