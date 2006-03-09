/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

volatile int *bar(void);

void f1(void)
{
  #pragma omp atomic
    *bar() += 1;
}

/* { dg-final { scan-tree-dump-times "__sync_fetch_and_add" 1 "gimple" { target i?86-*-* x86_64-*-* ia64-*-* powerpc*-*-* alpha*-*-* } } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
