/* { dg-do compile } */
/* { dg-options "-O3 -march=armv8.2-a+sve -ftrapping-math -frounding-math -fno-associative-math -fsigned-zeros -fdump-tree-ifcvt-details" } */

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

/* Rounding math prevents replacing the guarded addition with an addition
   of -0.0, but SVE can perform the addition only when the condition holds.  */
/* { dg-final { scan-tree-dump {\.COND_ADD \(} "ifcvt" } } */
/* { dg-final { scan-tree-dump-not {_ifc_.* = .* : -0.0;} "ifcvt" } } */
