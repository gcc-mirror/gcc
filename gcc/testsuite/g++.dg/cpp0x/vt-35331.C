// { dg-options "-std=c++0x" }
template<typename...> struct A;

template<typename...T> struct A<T*> // { dg-error "not expanded|T" }
{
  friend void foo();
};
