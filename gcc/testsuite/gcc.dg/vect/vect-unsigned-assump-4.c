/* { dg-do compile } */
/* { dg-additional-options "-fno-builtin" } */
/* { dg-require-effective-target vect_int } */

/* We should vectorize this without additional assumptions.
   The data-ref uses a pointer-based access which never wraps.  */

void f4 (int *p, unsigned start, unsigned end) {
  for (unsigned i = start; i != end; i++)
    *p++ = 0;
}

/* { dg-final { scan-tree-dump-not "adding no-wrap assumption" "vect" } } */
