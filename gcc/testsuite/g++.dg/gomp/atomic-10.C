// PR middle-end/28046
// { dg-do compile }
// { dg-options "-fopenmp -fdump-tree-gimple" }

int a[3], b;
struct C { int x; int y; } c;

int bar (void), *baz (void);

void
foo (void)
{
#pragma omp atomic
  a[2] += bar ();
#pragma omp atomic
  b += bar ();
#pragma omp atomic
  c.y += bar ();
#pragma omp atomic
  *baz () += bar ();
}

// { dg-final { scan-tree-dump-times "__sync_fetch_and_add" 4 "gimple" { target i?86-*-* x86_64-*-* ia64-*-* powerpc*-*-* alpha*-*-* } } }
// { dg-final { cleanup-tree-dump "gimple" } }
