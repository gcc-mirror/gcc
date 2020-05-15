// PR c+/57943
// { dg-do compile { target c++11 } }

struct a { };

template <typename T = decltype (a(0))> // { dg-error "" }
void f() { }
