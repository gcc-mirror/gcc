// PR c++/79583
// { dg-do compile { target c++11 } }

template < auto >  // { dg-error "parameter" "" { target c++14_down } }
struct Outer
{
  template < int >
  struct Inner { };
};
Outer<0> a{};
