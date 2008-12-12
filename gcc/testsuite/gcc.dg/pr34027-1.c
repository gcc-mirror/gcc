/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized" } */

unsigned long foobar(unsigned long ns)
{
  while(ns >= 10000L)
    ns -= 10000L;
  return ns;
}

/* This test was originally introduced to test that we transform
   to ns % 10000.  See the discussion of PR 32044 why we do not do
   that anymore.  */
/* { dg-final { scan-tree-dump-times "%" 0 "optimized" } } */
/* { dg-final { scan-tree-dump-times "/" 0 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
