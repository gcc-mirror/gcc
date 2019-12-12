// PR c++/71504
// { dg-do compile { target c++17 } }

typedef __SIZE_TYPE__ size_t;
template <typename T, T v>
struct integral_constant
{
  static constexpr T value = v;
  typedef T value_type;
  typedef integral_constant<T, v> type;
  constexpr operator value_type () const noexcept { return value; }
  constexpr value_type operator() () const noexcept { return value; }
};
template <typename T, T v>
constexpr T integral_constant<T, v>::value;
typedef integral_constant<bool, true> true_type;
typedef integral_constant<bool, false> false_type;
template <typename>
struct is_array : public false_type { };
template <typename T, size_t s>
struct is_array<T[s]> : public true_type { };
template <typename T>
struct is_array<T[]> : public true_type { };
template <bool, typename, typename>
struct conditional;
template <bool C, typename T, typename F>
struct conditional { typedef T type; };
template <typename T, typename F>
struct conditional<false, T, F> { typedef F type; };
template <typename T>
struct array_ref;
template <typename T>
using ref_t = typename conditional<is_array<T>::value, array_ref<T>, T&>::type;
template <typename T, unsigned N>
struct array_ref<T[N]>
{
  T *a;
  using const_reference = const ref_t<T>;
  constexpr const_reference operator[] (unsigned I) const { return {a[I]}; }
};
template <typename A>
array_ref (A&) -> array_ref<A>;
constexpr int a2[2] = {1,2};
static_assert (array_ref{a2}[0] == 1);
constexpr int a22[2][2] = {{1,2},{3,4}};
static_assert (array_ref{a22}[0][0] == 1);
