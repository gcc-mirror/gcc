/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */
/* { dg-require-effective-target cas_int } */

volatile int *bar(void);

void f1(void)
{
  #pragma omp atomic
    *bar() += 1;
}

/* { dg-final { scan-tree-dump-times "__atomic_fetch_add" 1 "ompexp" } } */
