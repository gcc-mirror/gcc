// { dg-do compile }
// { dg-options "-O2 -fdump-tree-cunrolli-details" }

template <typename T>
void
foo (T *a, T *b, T *c)
{
#pragma GCC unroll 8
  for (int i = 0; i < 8; i++)
    a[i] = b[i] * c[i];
}

void
bar (int *a, int *b, int *c)
{
  foo <int> (a, b, c);
}

// { dg-final { scan-tree-dump "loop with 8 iterations completely unrolled" "cunrolli" } }
