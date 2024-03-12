/* { dg-do compile } */
/* { dg-options "-Ofast -mcpu=neoverse-n2 -fdump-tree-vect-details -fno-tree-slp-vectorize" } */
/* { dg-final { scan-tree-dump "reduction latency = 8" "vect" } } */

/* The reduction latency should be multiplied by the count for
   single_defuse_cycle.  */

long
f (long res, short *ptr1, short *ptr2, int n)
{
  for (int i = 0; i < n; ++i)
    res += (long) ptr1[i] << ptr2[i];
  return res;
}
