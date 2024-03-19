// PR c++/112795
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-cunrolli-details" }

void baz (int);
constexpr int n = 3;
constexpr int m = 7;

template <typename T>
void
foo (int (&a)[3], T b)
{
#pragma GCC unroll(n)
  for (auto i : a)
    baz (i);
#pragma GCC unroll(m)
  for (auto i : b)
    baz (i);
}

template <int N>
void
bar (int (&a)[N])
{
#pragma GCC unroll(N)
  for (auto i : a)
    baz (i);
}

void
qux ()
{
  int a[3] = { 1, 2, 3 };
  int b[7] = { 4, 5, 6, 7, 8, 9, 10 };
  int c[6] = { 11, 12, 13, 14, 15, 16 };
  int d[10] = { 17, 18, 19, 20, 21, 22, 23, 24, 25, 26 };
  foo <int (&)[7]> (a, b);
  bar <6> (c);
  bar <10> (d);
}

// { dg-final { scan-tree-dump "loop with 3 iterations completely unrolled" "cunrolli" } }
// { dg-final { scan-tree-dump "loop with 6 iterations completely unrolled" "cunrolli" } }
// { dg-final { scan-tree-dump "loop with 7 iterations completely unrolled" "cunrolli" } }
// { dg-final { scan-tree-dump "loop with 10 iterations completely unrolled" "cunrolli" } }
