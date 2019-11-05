// Test with all operators explicitly defaulted.
// { dg-do run { target c++2a } }

#include <compare>

template <class T>
struct D
{
  T i;
  auto operator<=>(const D& x) const = default;
  bool operator==(const D& x) const = default;
  bool operator!=(const D& x) const = default;
  bool operator<(const D& x) const = default;
  bool operator<=(const D& x) const = default;
  bool operator>(const D& x) const = default;
  bool operator>=(const D& x) const = default;
};

template <class T>
struct E
{
  T i;
  auto operator<=>(const E& x) const = default;
  // auto operator==(const E& x) const = default;
  // auto operator!=(const E& x) const = default;
  // auto operator<(const E& x) const = default;
  // auto operator<=(const E& x) const = default;
  // auto operator>(const E& x) const = default;
  // auto operator>=(const E& x) const = default;
};

template <class T>
struct F
{
  T i;
  constexpr auto operator<=>(T x) const { return i<=>x; }
  constexpr bool operator== (T x) const { return i==x;  }
};

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

template <class T, class U>
constexpr bool check_eq (T d, U d2)
{
  return is_eq (d <=> d2)
    && is_eq (d2 <=> d)
    && is_lteq (d <=> d2)
    && is_lteq (d2 <=> d)
    && !is_lt (d <=> d2)
    && !is_lt (d2 <=> d)
    && is_gteq (d <=> d2)
    && is_gteq (d2 <=> d)
    && !is_gt (d <=> d2)
    && !is_gt (d2 <=> d)
    && d == d2
    && d2 == d
    && !(d != d2)
    && !(d2 != d)
    && d >= d2
    && d <= d2
    && d2 >= d
    && d2 <= d
    && !(d < d2)
    && !(d2 < d)
    && !(d > d2)
    && !(d2 > d);
}

template <class T, class U>
constexpr bool check_less (T d, U d2)
{
  return !is_eq (d <=> d2)
    && !is_eq (d2 <=> d)
    && is_lteq (d <=> d2)
    && !is_lteq (d2 <=> d)
    && is_lt (d <=> d2)
    && !is_lt (d2 <=> d)
    && !is_gteq (d <=> d2)
    && is_gteq (d2 <=> d)
    && !is_gt (d <=> d2)
    && is_gt (d2 <=> d)
    && !(d == d2)
    && !(d2 == d)
    && (d != d2)
    && (d2 != d)
    && !(d >= d2)
    && (d <= d2)
    && (d2 >= d)
    && !(d2 <= d)
    && (d < d2)
    && !(d2 < d)
    && !(d > d2)
    && (d2 > d);
}

int main()
{
  constexpr D<int> d{42};
  constexpr D<int> d2{24};

  static_assert (check_eq (d, d));
  static_assert (check_less (d2, d));

  constexpr E<float> e { 3.14 };
  constexpr E<float> ee { 2.72 };
  static_assert (check_eq (e, e));
  static_assert (check_less (ee, e));

  constexpr F<char> f { 'b' };
  static_assert (check_eq (f, 'b'));
  static_assert (check_less (f, 'c'));
  static_assert (check_less ('a', f));
}
