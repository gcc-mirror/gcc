/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized -Wno-psabi" } */

typedef int v4si __attribute__((vector_size(16)));

int f(v4si t)
{
  union {
      v4si t1;
      int t2[4];
  } u;
  u.t1 = t;
  return u.t2[1];
}

/* { dg-final { scan-tree-dump-not "u;" "optimized" } } */
/* { dg-final { scan-tree-dump-times "BIT_FIELD_REF" 1 "optimized" } } */
