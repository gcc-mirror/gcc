/* { dg-do compile } */
/* { dg-options "-O2 -mavx2 -fdump-tree-forwprop1" } */

typedef double v4df __attribute__((vector_size (32)));
typedef double v2df __attribute__((vector_size (16)));
typedef short v16hi __attribute__((vector_size (32)));
typedef short v8hi __attribute__((vector_size (16)));

v2df
foo (v4df x, double *p)
{
  return (v2df) { x[1], *p };
}

v2df
bar (v4df x, double *p)
{
  return (v2df) { x[0], *p }; /* BIT_INSERT_EXPR */
}

v2df
baz (v2df x, double *p)
{
  return (v2df) { x[1], *p }; /* VEC_PERM_EXPR */
}

v2df
qux (v2df x, double *p)
{
  return (v2df) { x[0], *p }; /* BIT_INSERT_EXPR */
}

v2df
corge (v4df x, double *p)
{
  return (v2df) { x[3], *p };
}

/* { dg-final { scan-tree-dump-times "BIT_INSERT_EXPR" 2 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 1 "forwprop1" } } */
/* We can't check for 1:1 assembler here so check for what we do not
   want to see.  */
/* { dg-final { scan-assembler-not { "perm" } } } */
/* { dg-final { scan-assembler-not { "insert" } } } */
/* { dg-final { scan-assembler-not { "broadcast" } } } */
