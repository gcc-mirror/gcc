/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-Ofast -fdump-tree-ifcvt-details" } */

float
f (const float *x, int n)
{
  float acc = 0;
  for (int i = 0; i < n; ++i)
    {
      float d = x[i];
      if (d < 0)
	acc += d;
      acc += d * 3.0f;
    }
  return acc;
}

/* { dg-final { scan-tree-dump "new phi replacement stmt" "ifcvt" } } */
/* { dg-final { scan-tree-dump "_ifc_.* = MIN_EXPR" "ifcvt" } } */
/* { dg-final { scan-tree-dump { = (acc_.* \+ _ifc_|_ifc_.* \+ acc_)} "ifcvt" } } */
