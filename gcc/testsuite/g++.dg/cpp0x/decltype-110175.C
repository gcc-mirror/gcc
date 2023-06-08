// PR c++/110175
// { dg-do compile { target c++11 } }

template<class T> auto f(T t) -> decltype(++t) { return t; } // { dg-warning "reference" "" { target c++14_down } }
void f(...) {}
void g() { f(true); }
