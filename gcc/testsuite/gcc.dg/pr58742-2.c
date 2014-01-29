/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

__SIZE_TYPE__
fx (char *a, __SIZE_TYPE__ sz)
{
  char *b = a + sz;
  /* The first forwprop pass should optimize this to return sz;  */
  return b - a;
}

/* { dg-final { scan-tree-dump "return sz" "cddce1" } } */
/* { dg-final { cleanup-tree-dump "cddce1" } } */
