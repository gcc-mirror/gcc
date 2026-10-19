/* { dg-do compile } */
/* { dg-require-effective-target vect_float } */
/* { dg-additional-options "-O3 -fno-associative-math -fsigned-zeros -fno-trapping-math -fdump-tree-ifcvt-details" } */

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

/* The inactive addend must preserve a negative zero in ACC.  */
/* { dg-final { scan-tree-dump "new phi replacement stmt" "ifcvt" } } */
/* { dg-final { scan-tree-dump "_ifc_.* = .* \\? .* : -0.0;" "ifcvt" } } */
