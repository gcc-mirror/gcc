/* PR tree-optimization/126896 */
/* { dg-do compile } */
/* { dg-options "-O2 --param=dom-jump-threading=0 -fdump-tree-optimized" } */

/* Even with DOM jump threading disabled, phiopt replicates the
   conditional into both predecessors and the boolean PHI dies.  */

void g (void);

void
f (int x, int a, int b, int c, int d)
{
  _Bool t;
  if (x)
    t = a < b;
  else
    t = c < d;
  if (t)
    g ();
}

/* { dg-final { scan-tree-dump-not "PHI" "optimized" } } */
