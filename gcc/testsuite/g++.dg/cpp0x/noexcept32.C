// PR c++/84045
// { dg-do compile { target c++11 } }

template <typename T> struct K {
  static const bool d = true;
};
template <typename T, typename> struct B {
  typedef K<T> D;
  void foo () noexcept (D::d);
};
template <typename T> struct P {
  P () noexcept (K<T>::d);
};
P<int> p;
