/* PR tree-optimization/49959 */
/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-phiopt1-details" } */

#define ABS(X)    (((X)>0)?(X):-(X))
unsigned long
test_abs(int *cur)
{
  unsigned long sad = 0;
  if (cur[0] > 0)
    sad = cur[0];
  else
    sad = -cur[0];
  return sad;
}

/* We should figure out that test_abs has an ABS_EXPR in it. */
/* { dg-final { scan-tree-dump " = ABS_EXPR" "phiopt1"} } */
/* { dg-final { scan-tree-dump-times "changed to factor operation out from" 1 "phiopt1"} } */

