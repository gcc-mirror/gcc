// PR c++/86355
// { dg-do compile { target c++11 } }

template <int...> struct integral_constant {
  static const int value = 1;
};
template <class... T> using mp_all = integral_constant<T::value...>;
template <class... T> using check2 = mp_all<mp_all<T..., integral_constant<0>>>;
check2<> x;

template <class T, class U> struct assert_same;
template <class T> struct assert_same<T,T> { };
assert_same<decltype(x),integral_constant<1>> a;
