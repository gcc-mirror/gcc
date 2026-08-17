/* { dg-do compile } */
/* { dg-options "-O2 --param dom-jump-threading=0 -fdump-tree-thread2-stats" } */
volatile unsigned sink;

void
f (int flag, unsigned n)
{
  unsigned i = flag ? 0 : n;
  do
    {
      sink = i;
      i += 4;
    }
  while (i != 4);
}

/* We should thread the path when i starts at 0 through loop exit.  */
/* { dg-final { scan-tree-dump "Jumps threaded: 1" "thread2" } } */
/* { dg-final { scan-tree-dump-times "sink" 2 "thread2" } } */
