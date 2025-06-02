/* { dg-do compile } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d --param=riscv-autovec-mode=V4QI -fdump-tree-vect-details" } */

/* By default we will use RVVM1SI mode for vectorization because N is not
   known.  Check that we use V4QI and create an epilogue when the autovec-mode
   param is specified.  */

void
foo (int *a, int *b, int n)
{
  for (int i = 0; i < n; i++)
    a[i] = b[i] + 1;
}

/* { dg-final { scan-tree-dump "Choosing vector mode V4QI" "vect" } } */
/* { dg-final { scan-tree-dump "Choosing epilogue vector mode RVVM1SI" "vect" } } */
