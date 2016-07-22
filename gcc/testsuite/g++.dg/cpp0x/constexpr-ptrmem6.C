// PR c++/71896
// { dg-do compile { target c++11 } }

struct Foo {
  int x;
};

constexpr bool compare(int Foo::*t) { return t == &Foo::x; }

constexpr bool b = compare(&Foo::x);

#define SA(X) static_assert ((X),#X)
SA(b);
