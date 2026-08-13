/* PR tree-optimization/126103 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-ethread -fdump-tree-threadfull1-details" } */

/* Like ssa-thread-abnormal-phi-1.c, but the conditional tests a value
   derived from the abnormal PHI.  */

void foo (void);
void bar (void);
int g;

void
f (int k)
{
  void *p = (k & 1) ? &&yes : &&join;
  int t;

  if (k > 0)
    {
      t = k;
      goto *p;
    }

  if (k < -5)
    {
      foo ();
      t = 1;
    }
  else
    {
      bar ();
      t = 0;
    }

join:
  if (g)
    bar ();
  if ((t & 2) == 0)
    {
yes:
      foo ();
    }
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 6 "threadfull1" } } */
