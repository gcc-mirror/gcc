// A version of alias-decl-79.C where defining-type-id of B and C
// are not dependent and instead their vector_size attribute is.
// PR c++/115897
// { dg-do compile { target c++11 } }

template<class T, class U>
struct is_same { static constexpr bool value = __is_same(T, U); };

#if __cpp_variable_templates
template<class T, class U>
constexpr bool is_same_v = __is_same(T, U);
#endif

template<class T> struct A;

template<int N>
void f() {
  using T = float;
  using B [[gnu::vector_size(N * sizeof(float))]] = T;
  static_assert(!is_same<T, B>::value, "");
  static_assert(!is_same<A<T>, A<B>>::value, "");
#if __cpp_variable_templates
  static_assert(!is_same_v<T, B>, "");
  static_assert(!is_same_v<A<T>, A<B>>, "");
#endif
};

template<int N>
void g() {
  using T = float*;
  using C [[gnu::vector_size(N * sizeof(float*))]] = T;
  static_assert(!is_same<T*, C>::value, "");
  static_assert(!is_same<A<T*>, A<C>>::value, "");
#if __cpp_variable_templates
  static_assert(!is_same_v<T*, C>, "");
  static_assert(!is_same_v<A<T*>, A<C>>, "");
#endif
};

template void f<4>();
template void g<4>();
