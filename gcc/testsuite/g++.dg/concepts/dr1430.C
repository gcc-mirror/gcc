// PR c++/66092
// { dg-do compile { target c++17_only } }
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
  concept bool Same()
  {
    return decltype(check<T, U, Args...>())::value;
  }

template <typename T, typename U, typename... Args>
  concept bool Similar = true;

template <typename... Args>
requires Same<Args...>() // { dg-error "" "" { xfail *-*-* } }
  void foo( Args... args ) {}
// FIXME: The new method of building concept checks is suppressing the
// diagnostic for the invalid substitution. This produces an invalid
// requires-clause, which still prevents the function from being resolved.

template <typename... Args>
requires Similar<Args...> // { dg-error "pack expansion" }
  void bar( Args... args ) {}

int main()
{
  foo(1, 2, 3); // { dg-error "" }
  bar(1, 2, 3); // { dg-error "" }
}
