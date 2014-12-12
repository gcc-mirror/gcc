// Test for range-based for with VLAs.
// { dg-do run { target c++11 } }
// { dg-options "-Wno-vla" }

#include <new>

void f(int i)
{
  int ar[i];
  int j = 0;
  for (int& x : ar)
    x = ++j;
  [&ar]{
    int k = 0;
    for (int x : ar)
      if (x != ++k)
	__builtin_abort();
  }();
}

int main()
{
  f(42);				// OK
}
