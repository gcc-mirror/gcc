/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-require-effective-target vect_condition } */
/* { dg-additional-options "-Ofast -fno-tree-reassoc -fdump-tree-ifcvt-details" } */

float
f (const float *x, const float *y, const float *z, int n)
{
  float acc = 0;
  for (int i = 0; i < n; ++i)
    {
      float d = x[i];
      if (d < 0)
	acc += d;
      acc += y[i];
      acc += z[i];
    }
  return acc;
}

/* Keep both unconditional additions, so the conditional PHI does not
   directly feed the statement that produces the latch value.  */
/* { dg-final { scan-tree-dump "Found cond scalar reduction" "ifcvt" } } */
/* { dg-final { scan-tree-dump "_ifc_.* = MIN_EXPR" "ifcvt" } } */
/* { dg-final { scan-tree-dump "vectorized 1 loops" "vect" } } */
