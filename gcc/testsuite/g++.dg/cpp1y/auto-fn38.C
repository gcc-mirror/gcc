// PR c++/80145
// { dg-do compile { target c++14 } }

auto* foo() { }  // { dg-error "no return statements" }
auto* foo();
