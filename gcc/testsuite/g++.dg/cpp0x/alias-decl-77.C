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
using B = T;

template<class T>
using C [[gnu::vector_size(16)]] = B<T>;

template<class T>
void f() {
  static_assert(!is_same<T, A<T>>::value, "");
  static_assert(is_same<C<T>, A<T>>::value, "");

#if __cpp_variable_templates
  static_assert(!is_same_v<T, A<T>>, "");
  static_assert(is_same_v<C<T>, A<T>>, "");
#endif
};

template void f<float>();
