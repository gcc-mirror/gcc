// { dg-do compile { target c++11 } }
template<typename... T> struct A
{
  template<T> struct B {}; // { dg-error "not expanded|T" }
};

A<int>::B<0> b;

template<typename... T> struct B
{
  template<T> B(); // { dg-error "not expanded|T" }
};

B<int> c;
