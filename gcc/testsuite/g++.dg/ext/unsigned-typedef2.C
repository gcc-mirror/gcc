// PR c++/108099
// { dg-do compile { target c++11 } }
// { dg-options "" }

typedef long long t64;
template <typename T, T v> struct integral_constant {
  static constexpr T value = v;
};
typedef integral_constant <bool, false> false_type;
typedef integral_constant <bool, true> true_type;
template <class T, class U>
struct is_same : false_type {};
template <class T>
struct is_same <T, T> : true_type {};

using s64 = signed t64;
static_assert (is_same <long long, s64>::value, "");
static_assert (is_same <signed long long, s64>::value, "");
static_assert (sizeof (s64) == sizeof (long long), "");
static_assert (s64(-1) < 0, "");

using u64 = unsigned t64;
static_assert (is_same <unsigned long long, u64>::value, "");
static_assert (sizeof (u64) == sizeof (unsigned long long), "");
static_assert (u64(-1) > 0, "");
