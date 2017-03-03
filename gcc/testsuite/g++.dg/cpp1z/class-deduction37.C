// { dg-options -std=c++1z }

template <class T> struct A
{
  using value_t = T;
  A(value_t);
};

template <class T>
A(typename A<T>::value_t) -> A<double>;

template <class,class> struct same;
template <class T> struct same<T,T> {};

A a(42);
same<decltype(a),A<double>> s1;
