// PR c++/60332
// { dg-do compile { target c++14 } }

void foo();

auto f = (auto(*)())(&foo);  // { dg-error "expected" }
// { dg-error "only available" "" { target c++20_down } .-1 }
