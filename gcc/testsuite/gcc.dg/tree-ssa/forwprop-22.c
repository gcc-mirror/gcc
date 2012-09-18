/* { dg-do compile } */
/* { dg-require-effective-target vect_double } */
/* { dg-require-effective-target vect_perm } */
/* { dg-options "-O -fdump-tree-optimized" } */

typedef double vec __attribute__((vector_size (2 * sizeof (double))));
void f (vec *px, vec *y, vec *z)
{
  vec x = *px;
  vec t1 = { x[1], x[0] };
  vec t2 = { x[0], x[1] };
  *y = t1;
  *z = t2;
}

/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "BIT_FIELD_REF" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
