// { dg-options "-std=c++0x" }
template<typename...> struct A;

template<typename...T, typename...U> 
  struct A<T..., U...> {}; // { dg-error "must be at the end" }

A<int> a; // { dg-error "incomplete" }
