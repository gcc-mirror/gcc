/* Verify that a warning about an exceeded Graphite parameter gets
   output as optimization information and not only as a dump message
   for OpenACC functions. */

/* { dg-additional-options "-O2 -fopt-info-missed --param=max-isl-operations=1" } */

void test (int* restrict a, int *restrict b)
{
  int i = 1;
  int j = 1;
  int m = 0;

#pragma acc parallel loop auto copyin(b) copyout(a) reduction(max:m)
/* { dg-missed {data-dependence analysis of OpenACC loop nest failed; try increasing the value of --param=max-isl-operations=1.} "" { target *-*-* } .-1  } */
/* { dg-missed {'auto' loop has not been analyzed \(cf. 'graphite' dumps for more information\).} "" { target *-*-* } .-2 } */
/* { dg-missed {.*not inlinable.*} "" { target *-*-* } .-3 } */
  for (i = 1; i < 995; i++)
    {
      int x = b[i] * 2;
      for (j = 1; j < 995; j++)
        m = m + a[i] + x;
    }
}
