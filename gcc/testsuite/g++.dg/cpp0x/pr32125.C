// { dg-options "-std=c++0x" }
template<typename...> struct A;

template<typename...T> struct A<T*> // { dg-error "not expanded|T" }
{       // { dg-error "not expanded|T" }
  A();  // { dg-error "not expanded|T" }
  A(T); // { dg-error "not expanded|T" }
};
