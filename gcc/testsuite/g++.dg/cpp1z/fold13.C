// { dg-do compile { target c++17 } }
// Verify constexpr evaluation of a large left fold logical expression
// isn't quadratic in the size of the expanded expression.

template<int> struct S { static constexpr bool value = true; };

template<class T, T...> struct integer_sequence { };

template<class T, T N>
using make_integer_sequence
#if __has_builtin(__make_integer_seq)
  = __make_integer_seq<integer_sequence, T, N>;
#else
  = integer_sequence<T, __integer_pack(N)...>;
#endif

template<int... Is>
constexpr bool f_impl(integer_sequence<int, Is...>) {
  return (... && S<Is>::value);
}

static_assert(f_impl(make_integer_sequence<int, 1024>()));

template<int... Is>
constexpr bool g_impl(integer_sequence<int, Is...>) {
  return (... || !S<Is>::value);
}

static_assert(!g_impl(make_integer_sequence<int, 1024>()));
