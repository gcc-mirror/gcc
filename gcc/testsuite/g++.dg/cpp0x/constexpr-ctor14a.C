// { dg-do compile { target c++11 } }
// { dg-options "-fno-elide-constructors" }

struct A
{
  void *p;
  constexpr A(): p(this) {}
};

constexpr A a;
constexpr A b = A();		// { dg-error "" }

#define SA(X) static_assert ((X), #X)
SA(a.p == &a);
