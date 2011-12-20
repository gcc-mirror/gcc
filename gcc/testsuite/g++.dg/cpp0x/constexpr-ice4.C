// PR c++/51612
// { dg-options -std=c++0x }

struct A {};

struct B : virtual A
{
  constexpr B() { } // { dg-error "has virtual base classes" }
};
