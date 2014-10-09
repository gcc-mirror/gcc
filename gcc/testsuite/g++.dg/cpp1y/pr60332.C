// PR c++/60332
// { dg-do compile { target c++14 } }

void foo();

auto f = (auto(*)())(&foo);  // { dg-error "invalid" }
