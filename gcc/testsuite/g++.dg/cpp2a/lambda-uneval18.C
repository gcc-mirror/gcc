// PR c++/116714
// PR c++/107390
// { dg-do compile { target c++20 } }

template<class T, class U>
inline constexpr bool is_same_v = __is_same(T, U);

template<class T, class U>
struct is_same { static constexpr bool value = false; };

template<class T>
struct is_same<T, T> { static constexpr bool value = true; };

template<class>
void f() {
  using type = decltype([]{});
  static_assert(is_same_v<type, type>);
  static_assert(is_same<type, type>::value);
};

template<class>
void g() {
  using ty1 = decltype([]{});
  using ty2 = ty1;
  static_assert(is_same_v<ty1, ty2>);
  static_assert(is_same<ty1, ty2>::value);
};

template<class>
void h() {
  using ty1 = decltype([]{});
  using ty2 = decltype([]{});
  static_assert(!is_same_v<ty1, ty2>);
  static_assert(!is_same<ty1, ty2>::value);
};

template void f<int>();
template void g<int>();
template void h<int>();
