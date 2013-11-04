// Test that we try normal init if no list ctor is viable.
// { dg-options "-std=c++11" }

#include <initializer_list>

struct B {};

struct C
{
  C(B);
};

struct A
{
  A(std::initializer_list<int>);
  A(B) { }
  A(C);
};

B b;
A a{b};
