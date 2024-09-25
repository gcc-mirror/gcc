// PR c++/66092
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

#include <type_traits>

template <typename T, typename U, typename... Args>
requires (sizeof...(Args) == 0)
  constexpr decltype(auto) check()
  {
    return std::integral_constant<bool, __is_same_as(T, U)>();
  }

template <typename T, typename U, typename... Args>
requires (sizeof...(Args) > 0)
  constexpr decltype(auto) check()
  {
    return std::integral_constant<bool, __is_same_as(T, U)
        && decltype(check<U, Args...>())::value>();
  }

template <typename T, typename U, typename... Args>
  concept Same =
    decltype(check<T, U, Args...>())::value;

template <typename T, typename U, typename... Args>
  concept Similar = true;

template <typename... Args>
requires Same<Args...>() // { dg-error "" }
  void foo( Args... args ) {}

template <typename... Args>
requires Similar<Args...> // { dg-error "pack expansion" }
  void bar( Args... args ) {}

int main()
{
  foo(1, 2, 3); // { dg-error "" }
  bar(1, 2, 3); // { dg-error "" }
}
