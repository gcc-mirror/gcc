// PR c++/58536
// { dg-do compile }
// { dg-options "-std=gnu++1y" }

struct A
{
  A(auto);
};

A::A(auto) {}
