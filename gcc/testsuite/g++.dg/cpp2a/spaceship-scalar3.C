// { dg-do run { target c++2a } }
// { dg-options "-fext-numeric-literals" }

#include <compare>

int main()
{
  // GCC complex literal extension  
  {
    constexpr auto v = 1 <=> 1i;
    static_assert (__is_same_as (decltype (v), const std::strong_equality));
    static_assert (!is_eq (v));
    static_assert (is_neq (v));
  }
  {
    constexpr auto v = 1i <=> 1.0i;
    static_assert (__is_same_as (decltype (v), const std::weak_equality));
    static_assert (is_eq (v));
    static_assert (!is_neq (v));
  }
}
