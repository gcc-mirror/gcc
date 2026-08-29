/* { dg-do compile } */
/* { dg-options "-O2 -fdisable-tree-phiopt1 -fdisable-tree-phiopt2 -fdisable-tree-phiopt3 -fdisable-tree-phiopt4 -fdump-tree-dom2-details" } */

/* Same test as dom-jump-threading-1.c but with DOM jump threading at
   its default (enabled): DOM must thread the PHI of compares, which
   keeps the sibling test honest.  */

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

/* { dg-final { scan-tree-dump "Registering jump thread" "dom2" } } */
