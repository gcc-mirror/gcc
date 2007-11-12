/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */

unsigned long foobar(unsigned long ns)
{
  while(ns >= 10000L)
    ns -= 10000L;
  return ns;
}

/* { dg-final { scan-tree-dump "ns % 10000" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
