// PR c++/55261
// { dg-options -std=c++11 }

struct A
{
};
struct B : A
{
  using A::A;
};
