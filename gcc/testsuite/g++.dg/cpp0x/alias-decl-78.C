// PR c++/115897
// { dg-do compile { target c++11 } }

template<class T, class U>
struct is_same { static constexpr bool value = __is_same(T, U); };

#if __cpp_variable_templates
template<class T, class U>
constexpr bool is_same_v = __is_same(T, U);
#endif

template<class T>
using A [[gnu::vector_size(16)]] = T;

template<class T>
void f() {
  using B = A<T>;
  static_assert(!is_same<T, B>::value, "");
#if __cpp_variable_templates
  static_assert(!is_same_v<T, B>, "");
#endif
};

template<class T>
void g() {
  using C = A<T*>;
  static_assert(!is_same<T*, C>::value, "");
#if __cpp_variable_templates
  static_assert(!is_same_v<T*, C>, "");
#endif
};

template void f<float>();
template void g<float>();
