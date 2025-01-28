/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-lim2-details" } */

char x;

long foo (int n)
{
  long y = 0;
  for (int j = 0; j < 1024; ++j)
    for (int i = 0; i < n; ++i)
      y += *(long *)&x;
  return y;
}

/* Because *(long *)&x may trap we have to preserve execution and
   only hoist it from the innermost loop (after the header check).  */
/* { dg-final { scan-tree-dump-not "out of loop 1" "lim2" } } */
/* { dg-final { scan-tree-dump "out of loop 2" "lim2" } } */
