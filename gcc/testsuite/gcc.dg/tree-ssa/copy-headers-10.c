/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ch-details -fdump-tree-ldist-details" } */

/* PR tree-optimization/122734 */
/* We want to duplicate the block after the one containing the condition going to unreachable.
   Since later on we will be removing the condition going to unreachable anyways. */
/* So in the end ldist can generate a memset. */

static inline int size(int *a)
{
  int t = *a;
  if (t < 0)  __builtin_unreachable();
  return t;
}

void f(int *l, short *d)
{
  for(int i = 0; i < size(l); i++)
    d[i] = 0;
}

/* { dg-final { scan-tree-dump-times "Duplicating bb . is a win" 1 "ch2" } } */
/* { dg-final { scan-tree-dump-times "Will duplicate bb" 2 "ch2" } } */
/* { dg-final { scan-tree-dump "is now do-while loop" "ch2" } } */
/* { dg-final { scan-tree-dump "generated memset zero" "ldist" } } */
