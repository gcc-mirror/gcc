// PR c++/52599
// { dg-options -std=c++11 }

struct foo {
  constexpr foo() try { } catch(...) { };  // { dg-error "constructor" }
};
