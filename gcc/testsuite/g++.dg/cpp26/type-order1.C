// C++26 P2830R10 - Constexpr Type Ordering
// { dg-do compile { target c++26 } }

namespace std {
  using type = enum _Ord { equivalent = 0, less = -1, greater = 1 };
  struct strong_ordering {
    type _M_value;
    constexpr strong_ordering (_Ord x) : _M_value (x) {}
    static const strong_ordering less;
    static const strong_ordering equal;
    static const strong_ordering greater;
    constexpr bool operator== (const strong_ordering &x) const { return _M_value == x._M_value; }
    constexpr bool operator!= (const strong_ordering &x) const { return _M_value != x._M_value; }
  };
  constexpr strong_ordering strong_ordering::equal (_Ord::equivalent);
  constexpr strong_ordering strong_ordering::less (_Ord::less);
  constexpr strong_ordering strong_ordering::greater (_Ord::greater);

  template <typename T, typename U>
  struct type_order
  {
    static constexpr strong_ordering value = __builtin_type_order (T, U);
  };

  template <typename T, typename U>
  inline constexpr strong_ordering type_order_v = __builtin_type_order (T, U);
}

struct S;
struct T;
template <typename T>
struct U
{
};
typedef int int2;
struct V {};
namespace
{
  struct W {};
}

template <typename T, typename U>
struct eq
{
  constexpr eq ()
  {
    static_assert (std::type_order <T, U>::value == std::strong_ordering::equal);
    static_assert (std::type_order <U, T>::value == std::strong_ordering::equal);
    static_assert (std::type_order_v <T, U> == std::strong_ordering::equal);
    static_assert (std::type_order_v <U, T> == std::strong_ordering::equal);
  }
};
template <typename T, typename U>
struct ne
{
  constexpr ne ()
  {
    static_assert (std::type_order <T, U>::value != std::strong_ordering::equal);
    static_assert (std::type_order <U, T>::value != std::strong_ordering::equal);
    static_assert (std::type_order <T, U>::value == std::strong_ordering::greater
		   ? std::type_order <U, T>::value == std::strong_ordering::less
		   : std::type_order <U, T>::value == std::strong_ordering::greater);
    static_assert (std::type_order_v <T, U> != std::strong_ordering::equal);
    static_assert (std::type_order_v <U, T> != std::strong_ordering::equal);
    static_assert (std::type_order_v <T, U> == std::strong_ordering::greater
		   ? std::type_order_v <U, T> == std::strong_ordering::less
		   : std::type_order_v <U, T> == std::strong_ordering::greater);
  }
};

constexpr eq <void, void> a;
constexpr eq <const void, const void> b;
constexpr eq <int, int> c;
constexpr eq <long int, long int> d;
constexpr eq <const volatile unsigned, const volatile unsigned> e;
constexpr eq <S, S> f;
constexpr eq <U <int>, U <int>> g;
constexpr eq <unsigned[2], unsigned[2]> h;
constexpr eq <int, int2> i;
constexpr eq <int (*) (int, long), int (*) (int, long)> j;
constexpr ne <int, long> k;
constexpr ne <const int, int> l;
constexpr ne <S, T> m;
constexpr ne <int &, int &&> n;
constexpr ne <U <S>, U <T>> o;
constexpr ne <U <short>, U <char>> p;
static_assert (std::type_order_v <S, T> != std::strong_ordering::less
	       || std::type_order_v <T, V> != std::strong_ordering::less
	       || std::type_order_v <S, V> == std::strong_ordering::less);
constexpr ne <int (*) (int, long), int (*) (int, int)> q;
constexpr eq <W, W> r;
constexpr ne <V, W> s;
constexpr eq <U <W>, U <W>> t;
constexpr ne <U <V>, U <W>> u;
