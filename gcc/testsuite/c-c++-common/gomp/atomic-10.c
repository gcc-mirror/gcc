/* PR middle-end/28046 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */
/* { dg-require-effective-target cas_int } */

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

/* { dg-final { scan-tree-dump-times "__atomic_fetch_add" 4 "ompexp" } } */
