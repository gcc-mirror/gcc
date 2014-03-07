// { dg-do compile { target c++11 } }
template<typename...> struct A;

template<typename...T> struct A<T*> // { dg-error "not expanded|T" }
{
  A();
  A(T);
};
