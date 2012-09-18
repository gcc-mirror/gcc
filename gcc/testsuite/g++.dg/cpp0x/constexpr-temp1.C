// { dg-do compile { target c++11 } }

struct A { int i; };
constexpr A f2 (const A& a) { return a; }
constexpr int f1 (const A &a) { return f2(a).i; }
A g(const A &a)
{
  return { f1(a) };
}
