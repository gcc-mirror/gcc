/* This test demonstrates a loop nest that Graphite cannot handle
   because of aliasing. It should be possible to handle this loop nest
   by creating a runtime alias check like in the very similar test
   alias-0-runtime-check.c. However Graphite analyses the data
   reference with respect to the innermost loop that contains the data
   reference, the variable "i" remains uninstantiated (in contrast to
   "j"), and consequently the alias check cannot be placed outside of
   the SCoP since "i" is not defined there. */

/* { dg-options "-O2 -fgraphite-identity -fgraphite-runtime-alias-checks -fdump-tree-graphite-details" } */

void sum(int *x, int *y, unsigned *sum)
{
  unsigned i,j;
  *sum = 0;

  for (i = 0; i < 10000; i=i+1)
    for (j = 0; j < 22222; j=j+1)
      *sum +=  x[i] + y[j];
}

/* { dg-final { scan-tree-dump "number of SCoPs: 1" "graphite" { xfail *-*-* } } } */
