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
constexpr int f0 (int n) { return n; }
consteval int f1 (int n) { return f0 (n) * n; }
consteval int f2 (int n) { return f1 (n); }
consteval bool f3 () { return std::is_constant_evaluated (); }
struct S { constexpr S (int x) : s (x) {} consteval int m1 (int n) const; int s; };
consteval int
S::m1 (int n) const
{
  n += s;
  return n;
}

constexpr int a = 2;
int b = f1 (a);
int c = f2 (f1 (a));
bool d = f3 ();
constexpr S e = 41;
int f = e.m1 (1);

int
main ()
{
  if (b != 4 || c != 16 || !d || f != 42)
    abort ();
}
