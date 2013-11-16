// { dg-do compile }
// { dg-options "-std=gnu++1y" }

// PR c++/58536

struct A
{
  A(auto);
};

A::A(auto) {}

