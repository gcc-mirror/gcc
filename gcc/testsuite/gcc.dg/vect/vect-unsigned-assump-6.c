/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-require-effective-target vect_gather_load_ifn } */

/* This should be vectorized without gather/scatter.  */

void f6 (int *__restrict a, int *__restrict b, unsigned start, unsigned end) {
  for (unsigned i = start; i <= end; i++)
    a[i] = b[i];
}

/* { dg-final { scan-tree-dump-not "gather" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
