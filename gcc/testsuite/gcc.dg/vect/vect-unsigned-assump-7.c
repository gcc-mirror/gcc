/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

/* This should be vectorized without gather/scatter.  */

void f7 (int *__restrict dst, unsigned n) {
  unsigned i = 1;
  while (i != n)
    {
      dst[i] = i;
      i++;
    }
}

/* { dg-final { scan-tree-dump-not "gather" "vect" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" } } */
/* { dg-final { scan-tree-dump "adding no-wrap assumption" "vect" { target { ! ilp32 } } } } */
