/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -fno-trapping-math -fdump-tree-ifcvt-details" } */

float
f (const float *x, int n)
{
  float acc = 0;
  for (int i = 0; i < n; ++i)
    {
      float d = x[i];
      if (d < 0)
	acc += d;
      acc = __builtin_fmaxf (acc, d * 3.0f);
    }
  return acc;
}

/* The latch update is a call, or a MAX_EXPR if the call is folded.  */
/* { dg-final { scan-tree-dump "new phi replacement stmt" "ifcvt" } } */
/* { dg-final { scan-tree-dump { = (acc_.* \+ _ifc_|_ifc_.* \+ acc_)} "ifcvt" } } */
/* { dg-final { scan-tree-dump "((__builtin_)?fmaxf|MAX_EXPR)" "ifcvt" } } */
