/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-cddce1" } */

/* PR tree-optimization/121921 */

int *
fx (int *b, int *e)
{
  __SIZE_TYPE__ p = b - e;
  /* The first forwprop pass should optimize this to return e;  */
  return b - p;
}

/* { dg-final { scan-tree-dump "return e" "cddce1" } } */

