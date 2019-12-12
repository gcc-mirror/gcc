// PR c++/88979
// { dg-do compile { target c++2a } }

template<typename T>
struct B {
  B(T::type);
};

template<typename T>
struct A {
  A(T::type);
};

template<typename T>
A<T>::A(T::type) { }
