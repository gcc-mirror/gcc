/* PR tree-optimization/127211 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-slp-vectorize -fdump-tree-ivopts-details" } */

int a[4096], b[4096];

void
fn (int m)
{
  int s = 0;
  for (int i = 0; i < a[4]; i++)
    {
      a[3*i + 2] = m;
      if (m & 1)
	a[3*i + 5] = m - 1;
      b[s] = m;
      s += a[4];
    }
}

/* Predictive commoning hoists the load of a[4] out of the loop, after
   which s is an affine IV.  SCEV results cached by its data-reference
   analysis before the transform must not reach ivopts.  */
/* { dg-final { scan-tree-dump-not "niter scev_not_known" "ivopts" } } */
