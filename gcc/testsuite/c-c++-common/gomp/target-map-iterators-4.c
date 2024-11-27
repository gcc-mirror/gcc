/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */
/* { dg-additional-options "-std=c++98" { target c++ } } */

int bar (int, int);
void baz (int, int *);
#pragma omp declare target enter (baz)

void
foo (int x, int *p)
{
  #pragma omp target map (iterator (i=0:4), to: p[bar (x, i)])
    baz (x, p);
}

/* { dg-final { scan-tree-dump "firstprivate\\\(x\\\)" "gimple" } } */
/* { dg-final { scan-tree-dump-times "bar \\\(x, i\\\)" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "map\\\(iterator\\\(int i=0:4:1, loop_label=" 2 "gimple" } } */
