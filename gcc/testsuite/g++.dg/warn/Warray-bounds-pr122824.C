// { dg-do compile }
// { dg-options "-O2 -std=gnu++23 -Warray-bounds" }

#include <format>

template <typename... Args>
auto foo(std::format_string<Args...> && format, Args &&... args)
{
  return std::format(std::move(format), std::forward<Args>(args)...);
}

int main()
{
  auto a = foo("{}", 42);
}
