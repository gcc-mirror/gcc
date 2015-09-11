/* PR middle-end/27388 */
/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

extern void baz (int);

void
foo (void)
{
  int i;
#pragma omp parallel for shared (i)
  for (i = 0; i < 2; i++)
    baz (i);
}

void
bar (void)
{
  int j = 0;
#pragma omp parallel shared (j)
  {
    j++;
#pragma omp for
    for (j = 0; j < 2; j++)
      baz (j);
  }
}

/* { dg-final { scan-tree-dump-times "shared\\\(i\\\)\[^\\n\]*private\\\(i\\\)" 0 "omplower" } } */
/* { dg-final { scan-tree-dump-times "private\\\(i\\\)\[^\\n\]*shared\\\(i\\\)" 0 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp for\[^\\n\]*private\\\(i\\\)" 1 "omplower" } } */
/* { dg-final { scan-tree-dump-times "shared\\\(j\\\)\[^\\n\]*private\\\(j\\\)" 0 "omplower" } } */
/* { dg-final { scan-tree-dump-times "private\\\(j\\\)\[^\\n\]*shared\\\(j\\\)" 0 "omplower" } } */
/* { dg-final { scan-tree-dump-times "omp for\[^\\n\]*private\\\(j\\\)" 1 "omplower" } } */
