// Origin PR c++/51462
// { dg-options "-std=c++11" }

struct A
{
  int i = 0;
};

struct B
{
  A a;
    constexpr B() : a(0) {} // { dg-error "no matching function" }
};
