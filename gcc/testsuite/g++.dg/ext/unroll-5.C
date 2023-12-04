// PR c++/112795
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-cunrolli-details" }

void baz (int);
constexpr int n = 3;

template <int N>
void
foo ()
{
#pragma GCC unroll(n)
  for (int i = 0; i != n; ++i)
    baz (i);
}

template <int N>
void
bar ()
{
#pragma GCC unroll(N)
  for (int i = 0; i != N; ++i)
    baz (i);
}

void
qux ()
{
  foo <2> ();
  bar <6> ();
  bar <10> ();
}

// { dg-final { scan-tree-dump "loop with 3 iterations completely unrolled" "cunrolli" } }
// { dg-final { scan-tree-dump "loop with 6 iterations completely unrolled" "cunrolli" } }
// { dg-final { scan-tree-dump "loop with 10 iterations completely unrolled" "cunrolli" } }
