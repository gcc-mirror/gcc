/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-O3 -fdump-tree-ifcvt-details" } */

int
f (const int *x, int n)
{
  int acc = 0;
  for (int i = 0; i < n; ++i)
    {
      int d = x[i];
      if (d < 0)
	acc += d;
      acc += d * 3;
    }
  return acc;
}

/* { dg-final { scan-tree-dump "new phi replacement stmt" "ifcvt" } } */
/* { dg-final { scan-tree-dump "_ifc_.* = MIN_EXPR" "ifcvt" } } */
/* { dg-final { scan-tree-dump { = (acc_.* \+ _ifc_|_ifc_.* \+ acc_)} "ifcvt" } } */
