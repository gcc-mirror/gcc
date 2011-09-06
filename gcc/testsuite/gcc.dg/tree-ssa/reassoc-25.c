/* { dg-do compile } */
/* { dg-options "-O2 --param tree-reassoc-width=3 -fdump-tree-reassoc1-details" } */

unsigned int
foo (int a, int b, int c, int d)
{
  unsigned int s = 0;

  s += a;
  s += b;
  s += c;
  s += d;

  return s;
}

/* Verify reassociation width was chosen to be 2.  */
/* { dg-final { scan-tree-dump-times "Width = 2" 1 "reassoc1"} } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
