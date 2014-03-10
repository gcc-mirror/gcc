// PR c++/55261
// { dg-do compile { target c++11 } }

struct A
{
};
struct B : A
{
  using A::A;
};
