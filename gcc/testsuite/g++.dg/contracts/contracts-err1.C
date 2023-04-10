// PR c++/108542
// { dg-additional-options -fcontracts }
// { dg-do compile { target c++11 } }

template<typename T>
void f (T n) {}
void g() [[pre: f]];			// { dg-error "overloaded" }
