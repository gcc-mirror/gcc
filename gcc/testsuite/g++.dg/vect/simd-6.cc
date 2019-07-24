// { dg-require-effective-target size32plus }
// { dg-additional-options "-fopenmp-simd" }
// { dg-additional-options "-mavx" { target avx_runtime } }
// { dg-final { scan-tree-dump-times "vectorized \[1-3] loops" 2 "vect" { target i?86-*-* x86_64-*-* } } }

#include "../../gcc.dg/vect/tree-vect.h"

template <typename T>
struct S {
  inline S ();
  inline ~S ();
  inline S (const S &);
  inline S & operator= (const S &);
  T s;
};

template <typename T>
S<T>::S () : s (0)
{
}

template <typename T>
S<T>::~S ()
{
}

template <typename T>
S<T>::S (const S &x)
{
  s = x.s;
}

template <typename T>
S<T> &
S<T>::operator= (const S &x)
{
  s = x.s;
  return *this;
}

template <typename T>
static inline void
ini (S<T> &x)
{
  x.s = 0;
}

S<int> r, a[1024], b[1024];

#pragma omp declare reduction (+: S<int>: omp_out.s += omp_in.s)
#pragma omp declare reduction (plus: S<int>: omp_out.s += omp_in.s) initializer (ini (omp_priv))

template <typename T>
__attribute__((noipa)) void
foo (S<T> *a, S<T> *b)
{
  #pragma omp simd reduction (inscan, +:r)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = r;
      #pragma omp scan exclusive(r)
      r.s += a[i].s;
    }
}

template <typename T>
__attribute__((noipa)) S<T>
bar (void)
{
  S<T> s;
  #pragma omp simd reduction (inscan, plus:s)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = s;
      #pragma omp scan exclusive(s)
      s.s += 2 * a[i].s;
    }
  return S<T> (s);
}

__attribute__((noipa)) void
baz (S<int> *a, S<int> *b)
{
  #pragma omp simd reduction (inscan, +:r) simdlen(1)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = r;
      #pragma omp scan exclusive(r)
      r.s += a[i].s;
    }
}

__attribute__((noipa)) S<int>
qux (void)
{
  S<int> s;
  #pragma omp simd if (0) reduction (inscan, plus:s)
  for (int i = 0; i < 1024; i++)
    {
      b[i] = s;
      #pragma omp scan exclusive(s)
      s.s += 2 * a[i].s;
    }
  return S<int> (s);
}

int
main ()
{
  S<int> s;
  check_vect ();
  for (int i = 0; i < 1024; ++i)
    {
      a[i].s = i;
      b[i].s = -1;
      asm ("" : "+g" (i));
    }
  foo (a, b);
  if (r.s != 1024 * 1023 / 2)
    abort ();
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i].s != s.s)
	abort ();
      else
	b[i].s = 25;
      s.s += i;
    }
  if (bar<int> ().s != 1024 * 1023)
    abort ();
  s.s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i].s != s.s)
	abort ();
      s.s += 2 * i;
    }
  r.s = 0;
  baz (a, b);
  if (r.s != 1024 * 1023 / 2)
    abort ();
  s.s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i].s != s.s)
	abort ();
      else
	b[i].s = 25;
      s.s += i;
    }
  if (qux ().s != 1024 * 1023)
    abort ();
  s.s = 0;
  for (int i = 0; i < 1024; ++i)
    {
      if (b[i].s != s.s)
	abort ();
      s.s += 2 * i;
    }
  return 0;
}
