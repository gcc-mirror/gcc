/* PR tree-optimization/79390 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -march=haswell -mtune=haswell -mfpmath=sse" } */
/* Make sure we emit a conditional move in this loop.  */

extern double A[32];

int
foo (void)
{
  double t = A[0];
  int jp = 0;
  int i;

  for (i = 0; i < 32; i++)
    {
      double ab = A[i];
      if (ab > t)
	{
	  jp = i;
	  t = ab;
	}
    }
 
  return jp;
}

/* { dg-final { scan-assembler "\[ \\t\]cmov\[a-z.]+\[ \\t\]" } } */
