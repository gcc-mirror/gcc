/* { dg-do compile } */
/* { dg-options "-O2 --param=dom-jump-threading=0 -fdisable-tree-phiopt1 -fdisable-tree-phiopt2 -fdisable-tree-phiopt3 -fdisable-tree-phiopt4 -fdump-tree-dom2-details -fdump-tree-optimized" } */

/* Verify that --param=dom-jump-threading=0 keeps DOM from threading
   jumps.  Only DOM can thread a PHI of compares (the backward
   threader cannot resolve the exit conditional to a single edge), so
   with the param off the join block and its PHI must survive.  */

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

/* { dg-final { scan-tree-dump-not "Registering jump thread" "dom2" } } */
/* { dg-final { scan-tree-dump "PHI <" "optimized" } } */
