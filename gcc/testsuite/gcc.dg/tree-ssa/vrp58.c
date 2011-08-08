/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

long long
foo (long long a, signed char b, signed char c)
{
  int bc = b * c;
  return a + (short)bc;
}

/* { dg-final { scan-tree-dump "Folded into" "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
