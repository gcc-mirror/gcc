// PR c++/101087
// { dg-do compile { target c++11 } }

int f();
static_assert(noexcept(sizeof(f())), "");
