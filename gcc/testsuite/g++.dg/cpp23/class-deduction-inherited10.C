// PR c++/122070
// { dg-do compile { target c++23 } }

template<class T> struct A { A(T) {}; };

using B = A<int>;

template<class T=void>
struct C : B { using B::B; };

C c = 0;
