/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-ch-details" } */

/* PR tree-optimization/122734 */
/* We want to duplicate the block after the one containing the
   condition since it is not the "true" header. */

static inline int size(int *a)
{
  int t = *a;
  if (t < 0) __builtin_unreachable();
  return t;
}

void f(int *l, short *d)
{
  for(int i = 0; i < size(l); i++)
  {
    if (d[i] < 0)
      return;
    d[i]--;
  }
  __builtin_abort ();
}

/* Not only we duplicate the true header but also duplicate the condition
   `d[i] < 0` but we should not. This structure does not show up enough to
   make a difference but we record it just in case we improve the heurstics
   some more.  */
/* { dg-final { scan-tree-dump-times "Duplicating bb . is a win" 1 "ch2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "Will duplicate bb" 2 "ch2" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump "is now do-while loop" "ch2" } } */
