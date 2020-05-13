// PR c++/88196
// { dg-do compile { target c++20 } }

struct C { C *c; };
template <C> struct D;
D <&C::c> d; // { dg-error "could not convert" }
