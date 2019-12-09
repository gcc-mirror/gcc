/* { dg-do compile } */
/* { dg-options "-O -msse2 -fdump-tree-forwprop1" } */

typedef double v2df __attribute__((vector_size (16)));

v2df
foo (v2df x, double *p)
{
  return (v2df) { x[0], *p };
}

v2df
bar (v2df x, double *p)
{
  return (v2df) { *p, x[1] };
}

/* { dg-final { scan-tree-dump-times "BIT_INSERT_EXPR" 2 "forwprop1" } } */
/* { dg-final { scan-assembler "movhpd" } } */
/* { dg-final { scan-assembler "movlpd" } } */
