// PR c++/9820

template <typename T> struct X {
  template<typename> static int test(...);
  template<typename> static int test(int *);

  static const int i = sizeof(X<T>::template test<int>(0));
};

template class X<int>;
