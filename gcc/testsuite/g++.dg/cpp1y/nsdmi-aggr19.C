// PR c++/100252
// { dg-do compile { target c++14 } }

#define SA(X) static_assert ((X),#X)

struct A {
  const A* p = this;
};

struct B {
  A a = (A{}, A{});
};

constexpr B b;
SA(b.a.p == &b.a);

struct C { 
  int x;
  int y = x;
};

struct D { 
  int x = 0;
  int y = (C{x}.y, C{x}.y);
};

constexpr D d = { };
D d2 = {};
