/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -fno-associative-math -frounding-math -fsigned-zeros -fno-trapping-math -fdump-tree-ifcvt-details" } */

float
f (const float *x, float init, int n)
{
  float acc = init;
  for (int i = 0; i < n; ++i)
    {
      float d = x[i];
      if (d < 0)
	acc += d;
      acc += d * 3.0f;
    }
  return acc;
}

/* With rounding math, an inactive addition of -0.0 need not preserve ACC.
   Disable trapping math so the addition reaches PHI conversion as an
   assignment, rather than being predicated using a conditional call.  */
/* { dg-final { scan-tree-dump "new phi replacement stmt" "ifcvt" } } */
/* { dg-final { scan-tree-dump {acc_[0-9]+ = [^\n]* \? acc_[0-9]+ : acc_[0-9]+;} "ifcvt" } } */
/* { dg-final { scan-tree-dump-not "Found cond scalar reduction" "ifcvt" } } */
