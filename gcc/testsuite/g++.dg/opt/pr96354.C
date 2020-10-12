// PR debug/96354
// { dg-do compile }
// { dg-options "-O2 -g -fopenmp-simd" }

template <int N> struct A { typedef double T[N]; };
template <int N> struct B { typename A<N>::T b; double *baz () { return b; } };
template <int N> struct C { B<N> d; C (); };
template <int N> C<N>::C () { double c = *d.baz (); }
template <int N> void operator- (C<N>, const C<N> &);
template <int> struct D {};
template <int N, int M> C<N> foo (D<N>, C<M>) { C<N> t; return t; }
int e;
struct E { D<3> d; void bar (); };

void
E::bar ()
{
#pragma omp simd
  for (int i = 0; i < e; i++)
    {
      C<3> f, g;
      g - foo (d, f);
    }
}
