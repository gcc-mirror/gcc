// PR c++/68754
// { dg-do compile { target c++11 } }

struct base { };
struct derived : base {
  constexpr derived& operator=(derived const&) = default; // { dg-error "defaulted declaration" "" { target { ! c++14 } } }
};
