// { dg-do compile { target c++20 } }

struct base { int i{}; };
struct derived : private base {};
template <derived> struct has_nttp_param{}; // { dg-error "non-type" }
has_nttp_param<{}> v;
