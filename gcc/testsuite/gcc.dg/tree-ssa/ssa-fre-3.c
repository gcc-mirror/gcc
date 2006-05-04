/* { dg-do compile } */
/* { dg-options "-O -fwrapv -fdump-tree-fre-details" } */

/* From PR14844.  */

int
foo (int a, int b)
{
  long long aa = a;
  long long bb = b;
  return aa + bb;
}

/* { dg-final { scan-tree-dump "Replaced \\\(int\\\) aa_.*with a_" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
