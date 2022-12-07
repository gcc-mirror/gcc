// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-options "-O2 -Warray-bounds" }

#include <algorithm>

static int bar(int n, int l)
{
  int f[l];
  int x = 0;
  int r = n;

  for (; x < l;)
    if (r)
      x = l;
    else
      r = 1;

  if (r == 1)
    std::sort(f, f + x, [](int a, int b) { return a > b; });
  return 1;
}

int foo(int n)
{
  return bar(n, 4);
}
