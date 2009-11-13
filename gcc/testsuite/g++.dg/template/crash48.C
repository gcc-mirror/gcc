// PR c++/11471
// Origin:  <bagnara@cs.unipr.it>
// { dg-do compile }

template<typename T> struct A
{
  typedef typename T::X X;
};

template<typename T> A<T>::X::X() {} // { dg-error "no type|invalid use|not a type|dependent" }
