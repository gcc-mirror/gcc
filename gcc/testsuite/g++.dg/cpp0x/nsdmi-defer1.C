// { dg-do compile { target c++11 } }

#define SA(X) static_assert(X,#X)

struct A
{
  int i = f();
  int j { f() };
  static constexpr int f() { return 42; }
};

constexpr A a;
SA(a.i == 42);
SA(a.j == 42);
