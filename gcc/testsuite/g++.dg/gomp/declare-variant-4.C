struct S { int a, b, c, d; };
void f1 (int) {}
void f1 (double) {}
template <typename T> void f2 (T) {}
void f3 (int) {}
#pragma omp declare variant (f1) match (user={condition(false)})
void f4 (int) {}
#pragma omp declare variant (::f1) match (user={condition(false)})
void f5 (const double) {}
#pragma omp declare variant (f2) match (user={condition(false)})
void f6 (int) {}
#pragma omp declare variant (f2) match (user={condition(false)})
void f6 (double) {}
#pragma omp declare variant (f2<long>) match (user={condition(false)})
void f6 (long) {}
#pragma omp declare variant (f3) match (user={condition(false)})
void f7 (int) {}
void f8 (int) {}
namespace N
{
  void f8 (int) {}
  #pragma omp declare variant (f3) match (user={condition(false)})
  void f9 (int) {}
  #pragma omp declare variant (f8) match (user={condition(false)})
  void f10 (int) {}
}
#pragma omp declare variant (f8) match (user={condition(false)})
void f11 (int) {}
void f12 (S, S &, int) {}
#pragma omp declare variant (f12) match (implementation={vendor(gnu)})
void f13 (const S, S &, const int) {}
// Try ADL
namespace M
{
  struct T { int a; };
  void f14 (T &, int) {}
}
#pragma omp declare variant (f14) match (implementation={vendor(gnu)})
void f15 (M::T &, int) {}
struct U
{
  void f16 (int, long) {}
  #pragma omp declare variant (f16) match (user={condition(false)})
  void f17 (int, long) {}
};
