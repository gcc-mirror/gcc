// PR c++/51621
// { dg-do compile { target c++11 } }

struct A
{
  A() {}
};

struct B
{
  A a[1];
  constexpr B() : a() {} // { dg-error "non-constant|non-.constexpr." }
};
