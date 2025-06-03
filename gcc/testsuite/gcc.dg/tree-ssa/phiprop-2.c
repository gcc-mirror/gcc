/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt2 -fdump-tree-phiprop1-details" } */

/* PR tree-optimization/116824 */

int g(int i, int *tt)
{
  const int t = 10;
  const int *a;
  {
    if (t < i)
    {
      *tt = 1;
      a = &t;
    }
    else
    {
      *tt = 1;
      a = &i;
    }
  }
  return *a;
}

/* Check that phiprop1 can do the insert of the loads. */
/* { dg-final { scan-tree-dump-times "Inserting PHI for result of load" 1 "phiprop1"} } */
/* Should be able to get MIN_EXPR in phiopt2 after cselim and phiprop. */
/* { dg-final { scan-tree-dump-times "MIN_EXPR " 1 "phiopt2" } } */
