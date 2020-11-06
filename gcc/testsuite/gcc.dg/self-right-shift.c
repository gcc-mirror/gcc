/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* Self right-shift should be optimized to 0. */

int
foo (int i)
{
  return i >> i;
}

/* { dg-final { scan-tree-dump "return 0;" "optimized" } } */
