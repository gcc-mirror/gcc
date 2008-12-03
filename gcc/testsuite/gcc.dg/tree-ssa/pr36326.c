/* { dg-do compile } */
/* { dg-options "-fdump-tree-gimple" } */

union X { int i; double x; };

int foo (union X *p)
{
  union X x = *p;
  return x.x;
}

/* { dg-final { scan-tree-dump-not "x.0" "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
