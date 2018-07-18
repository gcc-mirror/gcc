// PR c++/80145
// { dg-do compile { target c++14 } }

auto* foo() { return 0; }  // { dg-error "unable to deduce" }
auto* foo();
