// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fdump-tree-cunrolli-details" }

template <typename T>
void
foo (T (&a)[8], T *b, T *c)
{
#pragma GCC unroll 8
  for (int i : a)
    a[i] = b[i] * c[i];
}

void
bar (int (&a)[8], int *b, int *c)
{
  foo <int> (a, b, c);
}

// { dg-final { scan-tree-dump "loop with 8 iterations completely unrolled" "cunrolli" } }
