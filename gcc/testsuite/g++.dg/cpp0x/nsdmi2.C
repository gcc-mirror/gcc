// { dg-options -std=c++11 }

struct A
{
  int i;
  constexpr A(int i): i(i) {}
};

struct B
{
  A a1 = 1;
  A a2 { 2 };
  A a3 = { 3 };
};

#define SA(X) static_assert(X,#X)

constexpr B b;
SA(b.a1.i == 1);
SA(b.a2.i == 2);
SA(b.a3.i == 3);
