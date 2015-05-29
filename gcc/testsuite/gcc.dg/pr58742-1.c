/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

int *
fx (int *b, int *e)
{
  __SIZE_TYPE__ p = e - b;
  /* The first forwprop pass should optimize this to return e;  */
  return b + p;
}

/* { dg-final { scan-tree-dump "return e" "cddce1" } } */
