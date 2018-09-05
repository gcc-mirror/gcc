// PR c++/87185
// { dg-do compile { target c++11 } }

void f() { const int i=0; [&]() noexcept {i;}; }
