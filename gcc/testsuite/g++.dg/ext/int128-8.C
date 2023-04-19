// PR c++/108099
// { dg-do compile { target { c++11 && int128 } } }
// { dg-options "" }

using u128 = unsigned __int128_t;
using s128 = signed __int128_t;
template <typename T, T v> struct integral_constant {
  static constexpr T value = v;
};
typedef integral_constant <bool, false> false_type;
typedef integral_constant <bool, true> true_type;
template <class T, class U>
struct is_same : false_type {};
template <class T>
struct is_same <T, T> : true_type {};
static_assert (is_same <__int128, s128>::value, "");
static_assert (is_same <signed __int128, s128>::value, "");
static_assert (is_same <__int128_t, s128>::value, "");
static_assert (is_same <unsigned __int128, u128>::value, "");
static_assert (is_same <__uint128_t, u128>::value, "");
static_assert (sizeof (s128) == sizeof (__int128), "");
static_assert (sizeof (u128) == sizeof (unsigned __int128), "");
static_assert (s128(-1) < 0, "");
static_assert (u128(-1) > 0, "");
