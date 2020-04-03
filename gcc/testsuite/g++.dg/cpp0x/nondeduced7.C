// PR c++/58836
// { dg-do compile { target c++11 } }

template<typename, int> struct A;
template<typename T> struct A<T, T{}> {}; // { dg-error "partial specialization|involves template parameter" }
A<int, 0> a;
