/* PR tree-optimization/126103 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-ethread -fdump-tree-threadfull1-details" } */

/* The conditional at "join" tests the result of an abnormal PHI:
   t = PHI <k(ab), 0, 0>.  Both normal predecessors feed constant 0,
   so the backward threader must thread them past the conditional;
   the abnormal predecessor keeps the original block.  The two normal
   predecessors of the computed goto also know its destination, so
   they are threaded as well.  */

void foo (void);
void bar (void);

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
      t = 0;
    }
  else
    {
      bar ();
      t = 0;
    }

join:
  if (t != 0)
    {
yes:
      foo ();
    }
}

/* { dg-final { scan-tree-dump-times "Registering jump thread" 4 "threadfull1" } } */
