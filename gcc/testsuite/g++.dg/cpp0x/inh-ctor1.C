// { dg-do compile { target c++11 } }

struct A
{
  int i;
  constexpr A(int i): i(i) {}
};

struct B: A
{
  using A::A;
};

constexpr B b(42);

#define SA(X) static_assert((X),#X)
SA(b.i == 42);
