// PR c++/51612
// { dg-do compile { target c++11 } }

struct A {};

struct B : virtual A
{
  constexpr B() { } // { dg-error "has virtual base classes" }
};
