// PR c++/58536
// { dg-do compile { target c++14 } }
// { dg-additional-options "-fconcepts" }

struct A
{
  A(auto);
};

A::A(auto) {}
