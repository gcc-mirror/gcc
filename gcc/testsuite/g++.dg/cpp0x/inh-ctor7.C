// { dg-options "-std=c++11" }

struct A
{
  int i;
  template <class T>
  constexpr A(T t): i(t) {}
};

struct B: A
{
  using A::A;
};

constexpr B b(42);

#define SA(X) static_assert((X),#X)
SA(b.i == 42);
