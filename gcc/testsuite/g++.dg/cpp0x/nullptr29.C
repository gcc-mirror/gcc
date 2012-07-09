// PR c++/53882
// { dg-options "-std=gnu++11 -O" }

void f(decltype(nullptr) &__restrict np) { }
