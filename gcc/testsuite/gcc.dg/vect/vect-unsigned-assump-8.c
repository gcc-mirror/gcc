/* { dg-do compile } */
/* { dg-additional-options "-fno-builtin" } */
/* { dg-require-effective-target vect_int } */

/* We should vectorize this without additional assumptions.
   The data-ref uses a pointer-based access which never wraps.  */

void f8 (int *__restrict a, unsigned start, unsigned n) {
  for (unsigned i = start; i != n; i++)
    a[i - start] = 0;
}

/* { dg-final { scan-tree-dump-not "adding no-wrap assumption" "vect" } } */
