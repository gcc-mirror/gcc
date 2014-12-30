// { dg-do compile { target c++11 } }
template<typename ...T, int = 0> struct A {}; // { dg-error "end of" }

A<int> a;
