// PR c++/59111
// { dg-do compile { target c++11 } }

template<typename T> struct A : virtual T  // { dg-error "base type" }
{
  A();
  A(const A&);
};

template<typename T> A<T>::A(const A<T>&) = default;

A<int> a = A<int>();
