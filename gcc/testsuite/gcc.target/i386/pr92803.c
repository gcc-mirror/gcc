/* { dg-do compile } */
/* { dg-options "-O2 -Wno-psabi -mavx2 -fdump-tree-forwprop1" } */

typedef double v4df __attribute__((vector_size (32)));
typedef float v8sf __attribute__((vector_size (32)));
typedef float v4sf __attribute__((vector_size (16)));
typedef int v4si __attribute__((vector_size (16)));
typedef double v2df __attribute__((vector_size (16)));

v2df
foo (v4df x, double *p, v2df y)
{
  return (v2df) { x[3], *p };
}

v4sf
bar (v4si x, float *p)
{
  return (v4sf) { x[0], x[1], x[2], *p };
}

v4sf
baz (v4si x)
{
  return (v4sf) { x[0], x[1], 3.0f, 1.0f };
}

v4sf
barf (v8sf x)
{
  return (v4sf) { x[4], x[5], 1.0f, 2.0f };
}

/* For bar we do two inserts, first zero, then convert, then insert *p.  } */
/* { dg-final { scan-tree-dump-times "BIT_INSERT_EXPR" 2 "forwprop1" } } */
/* We expect all other CTORs to turn into permutes, the FP converting ones
   to two each with the one with constants possibly elided in the future
   by converting 3.0f and 1.0f "back" to integers.  */
/* For foo we do nothing.  */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 3 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "VEC_PERM_EXPR" 2 "forwprop1" { xfail *-*-* } } } */
