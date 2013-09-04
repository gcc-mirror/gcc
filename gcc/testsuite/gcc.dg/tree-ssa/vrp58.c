/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1-details" } */

long long
foo (long long a, signed char b, signed char c)
{
  int bc = b * c;
  return a + (short)bc;
}

/* { dg-final { scan-tree-dump "Folded into" "vrp1" { target int32plus } } } */
/* { dg-final { scan-tree-dump "Folding statement: _\[0-9\]\* = \\(long long int\\) bc_\[0-9\]\*;" "vrp1" { target int16 } } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
