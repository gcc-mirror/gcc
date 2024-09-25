// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-optimized" } */
// { dg-final { scan-tree-dump-times " bar<int> \\\(1, 2, 3\\\);" 1 "optimized" } }
// { dg-final { scan-tree-dump-times " bar<int> \\\(4, 5, 6\\\);" 1 "optimized" } }

template <typename T, typename U>
[[gnu::noipa]] U
foo (T x, T y, T z) [[gnu::unsequenced]]
{
  *x = 1;
  *y = 2;
  *z = 3;
}

template <typename T>
[[gnu::noipa]] T
bar (T x, T y, T z) [[gnu::unsequenced]]
{
  return x + y + z;
}

int
baz () [[gnu::unsequenced]]
{
  int x, y, z;
  foo <int *, void> (&x, &y, &z);
  return x;
}

int
qux () [[gnu::unsequenced]]
{
  int a = bar (1, 2, 3);
  int b = bar (1, 2, 3);
  int c = bar (1, 2, 3);
  int d = bar (4, 5, 6);
  int e = bar (4, 5, 6);
  int f = bar (4, 5, 6);
  return a + b + c + d + e + f;
}

template <typename T, typename U>
[[gnu::noipa]] U
corge (T x, T y, T z) [[gnu::unsequenced]]	// { dg-warning "'unsequenced' attribute on function type without pointer arguments returning 'void'" }
{
  x += y + z;
}

void
freddy ()
{
  corge <int, void> (1, 2, 3);
}
