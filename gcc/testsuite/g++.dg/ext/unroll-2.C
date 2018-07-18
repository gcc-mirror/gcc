// { dg-do compile }
// { dg-options "-O2 -fdump-tree-cunrolli-details" }
// { dg-skip-if "range for" { *-*-* } { "-std=gnu++98" } { "" } }

void
foo (int (&a)[8], int *b, int *c)
{
#pragma GCC unroll 8
  for (int i : a)
    a[i] = b[i] * c[i];
}

// { dg-final { scan-tree-dump "loop with 8 iterations completely unrolled" "cunrolli" } }
