/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */
/* { dg-require-effective-target cas_int } */

int *xyzzy;

void f1(void)
{
  #pragma omp atomic
    xyzzy++;
}

/* { dg-final { scan-tree-dump-times "xyzzy, 4" 1 "ompexp" } } */
/* { dg-final { cleanup-tree-dump "ompexp" } } */
