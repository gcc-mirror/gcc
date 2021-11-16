/* Verify that a warning about an exceeded Graphite parameter gets
   output as optimization information and not only as a dump message
   for OpenACC functions. */

/* { dg-additional-options "-O2 -fopt-info-missed --param=graphite-max-arrays-per-scop=1" } */

extern int a[1000];
extern int b[1000];

void test ()
{
#pragma acc parallel loop auto
/* { dg-missed {data-dependence analysis of OpenACC loop nest failed\; try increasing the value of --param=graphite-max-arrays-per-scop=1.} "" { target *-*-* } .-1  } */
/* { dg-missed {'auto' loop has not been analyzed \(cf. 'graphite' dumps for more information\).} "" { target *-*-* } .-2 } */
/* { dg-missed {.*not inlinable.*} "" { target *-*-* } .-3 } */
  for (int i = 1; i < 995; i++)
    a[i] = b[i + 5] + b[i - 1];
}


/* { dg-prune-output ".*not inlinable.*"} */
