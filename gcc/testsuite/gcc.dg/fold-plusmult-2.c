/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int foo (int i)
{
  return 2 + i * 4;
}

/* We do _not_ want the above to be canonicalized to (i * 2 + 1) * 2.  */

int bar (int i)
{
  return 4 + i * 2;
}

/* But eventually this to be canonicalized to (i + 2) * 2.  */

/* { dg-final { scan-tree-dump "i \\\* 4 \\\+ 2" "original" } } */
/* { dg-final { scan-tree-dump "\\\(i \\\+ 2\\\) \\\* 2" "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
