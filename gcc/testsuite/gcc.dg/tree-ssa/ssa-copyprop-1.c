/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-optimized" } */

typedef struct { int i; int j; } A;
int foo(A *a, int i)
{
  a->i = i;
  return a->i;
}

/* { dg-final { scan-tree-dump "return i" "optimized" } } */
