// { dg-do compile { target c++11 } }

void f(int) { }
auto f(bool) { return f(true); } // { dg-error "auto" }

void (*ptr)(int) = &f;
