// { dg-options -std=c++11 }

struct A
{
  int i;
  constexpr A(int, int i = num): i(i) {}
private:
  static const int num = 42;
};

struct B: A
{
  using A::A;
};

constexpr B b(24);

#define SA(X) static_assert((X),#X)
SA(b.i == 42);
