// PR c++/53882
// { dg-options "-O" }
// { dg-do compile { target c++11 } }

void f(decltype(nullptr) &__restrict np) { }
