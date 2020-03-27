// DR 1710 - Missing template keyword in class-or-decltype
// { dg-do compile { target c++14 } }

template <typename T> struct S {
  template <typename TT>
  using U = TT;
};
template <typename T> typename S<int>::template U<T>::type foo;
