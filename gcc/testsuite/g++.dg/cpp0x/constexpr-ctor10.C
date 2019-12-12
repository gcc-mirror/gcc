// PR c++/52599
// { dg-do compile { target c++11 } }

struct foo {
  constexpr foo() try { } catch(...) { };  // { dg-error "constexpr" "" { target c++17_down } }
};
