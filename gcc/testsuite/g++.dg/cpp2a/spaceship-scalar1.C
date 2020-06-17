// { dg-do run { target c++20 } }

#include <compare>

#define assert(X) do { if (!(X)) __builtin_abort(); } while(0)

int main()
{
  {
    constexpr auto v = 1 <=> 2;
    static_assert (__is_same_as (decltype (v), const std::strong_ordering));
    static_assert (!is_eq (v));
    static_assert (is_neq (v));
    static_assert (is_lt (v));
    static_assert (is_lteq (v));
    static_assert (!is_gt (v));
    static_assert (!is_gteq (v));
  }

  {
    enum E { a = 0 };
    constexpr auto v = E::a <=> 1;
    static_assert (__is_same_as (decltype (v), const std::strong_ordering));
    static_assert (!is_eq (v));
    static_assert (is_neq (v));
    static_assert (is_lt (v));
    static_assert (is_lteq (v));
    static_assert (!is_gt (v));
    static_assert (!is_gteq (v));
  }

  {
    enum class E { a, b };
    constexpr auto v = E::a <=> E::b;
    static_assert (__is_same_as (decltype (v), const std::strong_ordering));
    static_assert (!is_eq (v));
    static_assert (is_neq (v));
    static_assert (is_lt (v));
    static_assert (is_lteq (v));
    static_assert (!is_gt (v));
    static_assert (!is_gteq (v));
  }

  {
    int ar[2];
    constexpr auto v = &ar[1] <=> &ar[0];
    static_assert (__is_same_as (decltype (v), const std::strong_ordering));
    static_assert (!is_eq (v));
    static_assert (is_neq (v));
    static_assert (!is_lt (v));
    static_assert (!is_lteq (v));
    static_assert (is_gt (v));
    static_assert (is_gteq (v));
  }

  {
    constexpr auto v = 3.14 <=> 3.14;
    static_assert (__is_same_as (decltype (v), const std::partial_ordering));
    static_assert (is_eq (v));
    static_assert (!is_neq (v));
    static_assert (!is_lt (v));
    static_assert (is_lteq (v));
    static_assert (!is_gt (v));
    static_assert (is_gteq (v));
  }
}
