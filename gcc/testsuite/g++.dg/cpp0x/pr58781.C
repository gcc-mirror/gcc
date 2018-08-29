// PR c++/58781
// { dg-do compile { target c++11 } }

#include <cstddef>

int
operator""_s(const char32_t *a, size_t b)
{
  return 0;
}

void
f()
{
  using a = decltype(U"\x1181"_s);
  using b = decltype(U"\x8111"_s);
  using c = decltype(U" \x1181"_s);
}
