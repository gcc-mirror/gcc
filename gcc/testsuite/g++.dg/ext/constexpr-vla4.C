// PR c++/70452
// { dg-do compile { target c++14 } }

constexpr int
foo (int n, bool p)
{
  __extension__ int a [n] = { 0 };
  if (n == 3)
    foo (n - 2, false);
  if (n == 3)
    foo(n - 1, true);
  if (p)
    return a[1];
  return 0;
}

constexpr int i2 = foo (3, false); // { dg-bogus "array subscript out of bound" }
