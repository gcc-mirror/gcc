// PR c++/106793

template <class T> struct A { A(T); };
template <class T> void f(A *a); // { dg-error "placeholder.*parameter" "" { target c++17 } }
// { dg-error "" "" { target c++14_down } .-1 }
