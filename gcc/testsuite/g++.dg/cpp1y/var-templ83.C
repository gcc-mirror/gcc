// PR c++/110580
// { dg-do compile { target c++14 } }

template<class T>
struct A {
  template<typename U, typename V = U>
  static constexpr bool v1 = __is_same(U, V);

  template<typename U, typename V = T>
  static constexpr bool v2 = !__is_same(U, V);

  static_assert(v1<int>, "");
  static_assert(v2<int>, "");
};

template struct A<char>;
