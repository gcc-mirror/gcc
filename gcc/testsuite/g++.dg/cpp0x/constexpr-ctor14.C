// { dg-do compile { target c++11 } }

struct A
{
  void *p;
  constexpr A(): p(this) {}
};

constexpr A a;
constexpr A b = A();

#define SA(X) static_assert ((X), #X)
SA(a.p == &a);
SA(b.p == &b);
