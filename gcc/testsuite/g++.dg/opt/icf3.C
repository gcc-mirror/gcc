// Test that -fipa-icf combines the backing arrays for a and b.
// { dg-do run { target c++11 } }
// { dg-options -fipa-icf }

#include <initializer_list>

template <class T>
[[gnu::noipa]] void f (std::initializer_list<T> a,
		       std::initializer_list<T> b)
{
  if (a.begin() != b.begin()) __builtin_abort();
}

int main()
{
  f ({1,2}, {1,2});
}
