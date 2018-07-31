// PR c++/85792
// { dg-do compile { target c++11 } }
// { dg-additional-options -Wctor-dtor-privacy }

template<typename T> struct A { };

template<typename T> struct B : A<T> {
  using A<T>::A;

  B(const B&) { }
};
