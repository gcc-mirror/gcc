// { dg-do run }
// { dg-options "-std=c++2a" }

namespace std {
  constexpr inline bool
  is_constant_evaluated () noexcept
  {
    return __builtin_is_constant_evaluated ();
  }
}

extern "C" void abort ();
template <int N>
constexpr int f0 (int n) { return n + N; }
template <int N>
consteval int f1 (int n) { return f0<N> (n) * n + N; }
template <int N>
consteval int f2 (int n) { return f1<N> (n); }
template <int N>
consteval bool f3 () { return std::is_constant_evaluated () + N; }
struct S { constexpr S (int x) : s (x) {} template <int N> consteval int m1 (int n) const; int s; };
template <int N>
consteval int
S::m1 (int n) const
{
  n += s + N;
  return n;
}

constexpr int a = 2;
int b = f1<0> (a);
int c = f2<0> (f1<0> (a));
bool d = f3<0> ();
constexpr S e = 41;
int f = e.m1<0> (1);

int
main ()
{
  if (b != 4 || c != 16 || !d || f != 42)
    abort ();
}
