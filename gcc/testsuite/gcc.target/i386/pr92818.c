/* { dg-do compile } */
/* { dg-options "-O -mavx2 -fdump-tree-forwprop1" } */

typedef double v4df __attribute__((vector_size (32)));
typedef double v2df __attribute__((vector_size (16)));

v2df
bar (v4df x, double *p)
{
  return (v2df) { x[0], *p };
}

/* { dg-final { scan-tree-dump "BIT_INSERT_EXPR" "forwprop1" } } */
/* { dg-final { scan-assembler "movhpd" } } */
