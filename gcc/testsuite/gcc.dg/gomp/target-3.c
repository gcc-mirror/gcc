/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

struct S
{
  int a, b;
};

void foo (struct S *s)
{
  #pragma omp target map (alloc: s->a, s->b)
    ;
  #pragma omp target enter data map (alloc: s->a, s->b)
}

/* { dg-final { scan-tree-dump-times "map\\(struct:\\*s \\\[len: 2\\\]\\) map\\(alloc:s->a \\\[len: \[0-9\]+\\\]\\) map\\(alloc:s->b \\\[len: \[0-9\]+\\\]\\)" 2 "gimple" } } */
