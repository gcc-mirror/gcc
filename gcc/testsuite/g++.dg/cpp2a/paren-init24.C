// PR c++/94149 - make __is_constructible work with paren-init of aggrs.
// { dg-do compile { target c++20 } }

#include <type_traits>

int main()
{
  using T = int[1];
  T t(1);

  static_assert(__is_constructible(T, int));
  static_assert(!__is_constructible(T, int, int));
  static_assert(std::is_constructible_v<T, int>);
  static_assert(std::is_nothrow_constructible_v<T, int>);

  using T2 = int[2];
  T2 t2(1);
  T2 t3(1, 2);

  static_assert(__is_constructible(T2, int));
  static_assert(__is_constructible(T2, int, int));
  static_assert(std::is_constructible_v<T2, int, int>);
  static_assert(std::is_nothrow_constructible_v<T2, int, int>);
}
