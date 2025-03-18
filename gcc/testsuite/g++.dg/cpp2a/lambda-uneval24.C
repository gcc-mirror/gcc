// PR c++/119134
// { dg-do compile { target c++20 } }

void f(auto... args) requires(([args] {}, ..., true)) {}
