/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

volatile unsigned sink;

void
f (int flag, unsigned n)
{
  unsigned i = 0;
  do
    {
      sink = i;
      i += 1;
    }
  while (i != 128);
}

/* We should not peel this loop.  */
/* { dg-final { scan-tree-dump-times "sink" 1 "optimized" } } */
