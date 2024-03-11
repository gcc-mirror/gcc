// PR c++/109966
// { dg-do compile { target c++14 } }

#define SA(X) static_assert ((X),#X)

struct A {
  int a;
  int b = a;
};

struct B {
  int x = 0;
  int y[1]{A{x}.b};
};

constexpr B b = { };
SA(b.y[0] == 0);
