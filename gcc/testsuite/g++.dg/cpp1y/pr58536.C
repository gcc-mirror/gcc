// PR c++/58536
// { dg-do compile { target c++14 } }
// { dg-options "" }

struct A
{
  A(auto);
};

A::A(auto) {}
