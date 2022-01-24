// { dg-do compile { target c++11 } }

struct A
{
  int : 8;
  int i = 42;
};

constexpr A a;

struct B : A {
};

constexpr B b;

struct C {
  int : 0;
};

constexpr C c;
