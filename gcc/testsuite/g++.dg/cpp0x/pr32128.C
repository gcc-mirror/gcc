// { dg-do compile { target c++11 } }
template<typename...> struct A;

template<typename...T, typename...U> 
  struct A<T..., U...> {}; // { dg-error "must be at the end" }

A<int> a; // { dg-error "incomplete" }
