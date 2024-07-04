// PR c++/109899
// { dg-do compile { target c++11 } }

struct A { A(); ~A(); };

template<class T>
using array = T[42];

template<class T>
void f() {
  array<A>{};
}
