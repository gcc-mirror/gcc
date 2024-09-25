// { dg-do compile { target c++11 } }

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wc++20-extensions"

void f(auto p) { }
