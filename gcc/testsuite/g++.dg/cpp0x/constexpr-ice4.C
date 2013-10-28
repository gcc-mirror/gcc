// PR c++/51612
// { dg-options -std=c++11 }

struct A {};

struct B : virtual A
{
  constexpr B() { } // { dg-error "has virtual base classes" }
};
