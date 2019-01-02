// PR c++/88196
// { dg-do compile { target c++2a } }

struct C { C *c; };
template <C> struct D;
D <&C::c> d; // { dg-error "could not convert" }
