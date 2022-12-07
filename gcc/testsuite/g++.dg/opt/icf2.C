// Test that -fipa-icf combines the backing arrays for a and b.
// { dg-do run { target c++11 } }
// { dg-options -fipa-icf }

#include <initializer_list>

[[gnu::noipa]] void f (const void *a, const void *b)
{
  if (a != b) __builtin_abort();
}

int main()
{
  auto a = { 1, 2 };
  auto b = { 1, 2 };
  f (a.begin(), b.begin());
}
