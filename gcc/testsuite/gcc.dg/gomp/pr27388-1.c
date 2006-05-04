/* PR middle-end/27388 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

int n, o;

void
foo (void)
{
#pragma omp parallel firstprivate (n)
  {
    int i;
#pragma omp parallel for firstprivate (n)
    for (i = 0; i < 10; i++)
      ++n;
#pragma omp atomic
    o += n;
  }
}

/* { dg-final { scan-tree-dump-times "shared\\\(i\\\)" 0 "omplower" } } */
/* { dg-final { scan-tree-dump-times "private\\\(i\\\)" 1 "omplower" } } */
/* { dg-final { cleanup-tree-dump "omplower" } } */
