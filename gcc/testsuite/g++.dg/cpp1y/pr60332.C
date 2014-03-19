// PR c++/60332
// { dg-do compile { target c++1y } }

void foo();

auto f = (auto(*)())(&foo);  // { dg-error "invalid" }
