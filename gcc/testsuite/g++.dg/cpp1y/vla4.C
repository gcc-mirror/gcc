// Test for range-based for with VLAs.
// { dg-options -std=c++1y }
// { dg-do run }

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
