/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

int *xyzzy;

void f1(void)
{
  #pragma omp atomic
    xyzzy++;
}

/* { dg-final { scan-tree-dump-times "xyzzy, 4" 1 "gimple" { target i?86-*-* x86_64-*-* ia64-*-* powerpc*-*-* alpha*-*-* } } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
