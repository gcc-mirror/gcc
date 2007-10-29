/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */

int *xyzzy;

void f1(void)
{
  #pragma omp atomic
    xyzzy++;
}

/* { dg-final { scan-tree-dump-times "xyzzy, 4" 1 "ompexp" { target i?86-*-* x86_64-*-* ia64-*-* powerpc*-*-* alpha*-*-* } } } */
/* { dg-final { cleanup-tree-dump "ompexp" } } */
