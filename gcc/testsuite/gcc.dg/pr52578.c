/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

long bar (long i)
{
  return (long)((unsigned long)i + 2) - (long)i;
}
long foo (int i)
{
  return (long)((unsigned long)i + 2) - (long)i;
}

/* { dg-final { scan-tree-dump-times "return 2;" 2 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
