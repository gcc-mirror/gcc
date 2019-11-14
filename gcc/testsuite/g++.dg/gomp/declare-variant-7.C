// Test parsing of #pragma omp declare variant
// { dg-do compile }
// { dg-additional-options "-fdump-tree-gimple" }

void f01 ();
#pragma omp declare variant (f01) match (user={condition(1)},device={arch(x86_64)})
template <int N>
void f02 ();
void f03 ();
#pragma omp declare variant (f03) match (user={condition(score(N+2):N)})
template <int N>
void f04 ();
template <int N>
void f05 ();
#pragma omp declare variant (f05<N>) match (user={condition((T)N)},implementation={vendor("gnu")})
template <int N, typename T>
void f06 ();
void f07 ();
#pragma omp declare variant (f07) match (user={condition(score(N+2):N)})
template <int N>
void f08 ();
template <int N>
void f09 ();
#pragma omp declare variant (f09<N>) match (user={condition((T) N)})
template <int N, typename T>
void f10 ();
template <int N>
struct S
{
  template <typename T>
  void f11 (T) {}
  #pragma omp declare variant (f11<T>) match (user={condition(score(N):N)})
  template <typename T>
  void f12 (T) {}
  template <typename T>
  void f13 (T);
  #pragma omp declare variant (f13<T>) match (user={condition(score(N):N)})
  template <typename T>
  void f14 (T);
  int s;
};
template <int N>
struct T
{
  template <typename T>
  void f15 (T) {}
  #pragma omp declare variant (f15<T>) match (user={condition(score(N):N)})
  template <typename T>
  void f16 (T) {}
  template <typename T>
  void f17 (T);
  #pragma omp declare variant (f17<T>) match (user={condition(score(N):N)})
  template <typename T>
  void f18 (T);
  int t;
};

void
test ()
{
  f02<1> ();	// { dg-final { scan-tree-dump-times "f01 \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && lp64 } } } }
		// { dg-final { scan-tree-dump-times "f02<1> \\\(\\\);" 1 "gimple" { target { { i?86-*-* x86_64-*-* } && { ! lp64 } } } } }
		// { dg-final { scan-tree-dump-times "f02<1> \\\(\\\);" 1 "gimple" { target { ! { i?86-*-* x86_64-*-* } } } } }
  f04<1> ();		// { dg-final { scan-tree-dump-times "f03 \\\(\\\);" 1 "gimple" } }
  f06<1, long> ();	// { dg-final { scan-tree-dump-times "f05<1> \\\(\\\);" 1 "gimple" } }
  f08<0> ();		// { dg-final { scan-tree-dump-times "f08<0> \\\(\\\);" 1 "gimple" } }
  f10<0, short int> ();	// { dg-final { scan-tree-dump-times "f10<0, short int> \\\(\\\);" 1 "gimple" } }
  S<1> s;
  s.f12 (0);		// { dg-final { scan-tree-dump-times "S<1>::f11<int> \\\(&s, 0\\\);" 1 "gimple" } }
  s.f12 (0.0);		// { dg-final { scan-tree-dump-times "S<1>::f11<double> \\\(&s, 0.0\\\);" 1 "gimple" } }
  s.f14 (0LL);		// { dg-final { scan-tree-dump-times "S<1>::f13<long long int> \\\(&s, 0\\\);" 1 "gimple" } }
  T<0> t;
  t.f16 (s);		// { dg-final { scan-tree-dump-times "T<0>::f16<S<1> > \\\(&t, s\\\);" 1 "gimple" } }
  t.f18 (s);		// { dg-final { scan-tree-dump-times "T<0>::f18<S<1> > \\\(&t, s\\\);" 1 "gimple" } }
}
