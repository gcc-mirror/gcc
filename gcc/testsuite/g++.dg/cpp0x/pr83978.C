// PR c++/83978
// { dg-do compile { target c++11 } }

template <int N>
struct A { A () { auto a = [] { enum E { F }; }; } };
A<0> *p = new A<0> ();
