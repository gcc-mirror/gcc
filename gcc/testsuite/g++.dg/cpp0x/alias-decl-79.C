// PR c++/115897
// { dg-do compile { target c++11 } }

template<class T, class U>
struct is_same { static constexpr bool value = __is_same(T, U); };

#if __cpp_variable_templates
template<class T, class U>
constexpr bool is_same_v = __is_same(T, U);
#endif

template<class T> struct A;

template<class T>
void f() {
  using B [[gnu::vector_size(16)]] = T;
  static_assert(!is_same<T, B>::value, "");        // { dg-bogus "" "" { xfail *-*-* } }
  static_assert(!is_same<A<T>, A<B>>::value, "");  // { dg-bogus "" "" { xfail *-*-* } }
#if __cpp_variable_templates
  static_assert(!is_same_v<T, B>, "");             // { dg-bogus "" "" { xfail c++14 } }
  static_assert(!is_same_v<A<T>, A<B>>, "");       // { dg-bogus "" "" { xfail c++14 } }
#endif
};

template<class T>
void g() {
  using C [[gnu::vector_size(16)]] = T*;
  static_assert(!is_same<T*, C>::value, "");       // { dg-bogus "" "" { xfail *-*-* } }
  static_assert(!is_same<A<T*>, A<C>>::value, ""); // { dg-bogus "" "" { xfail *-*-* } }
#if __cpp_variable_templates
  static_assert(!is_same_v<T*, C>, "");            // { dg-bogus "" "" { xfail c++14 } }
  static_assert(!is_same_v<A<T*>, A<C>>, "");      // { dg-bogus "" "" { xfail c++14 } }
#endif
};

template void f<float>();
template void g<float>();
