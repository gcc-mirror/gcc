/* Skip on MIPS, where LOGICAL_OP_NON_SHORT_CIRCUIT inhibits the setcc
   optimizations that expose the VRP opportunity.  */
/* Skip on S/390 and avr.  Lower values in BRANCH_COST lead to two conditional
   jumps when evaluating an && condition.  VRP is not able to optimize
   this.  */
/* { dg-do compile { target { ! "mips*-*-* s390*-*-*  avr-*-*" } } } */
/* { dg-options "-O2 -fdump-tree-vrp -fdump-tree-dom" } */

int h(int x, int y)
{
  if ((x >= 0 && x <= 1) && (y >= 0 && y <= 1))
    return x && y;
  else
    return -1;
}

int g(int x, int y)
{
  if ((x >= 0 && x <= 1) && (y >= 0 && y <= 1))
    return x || y;
  else
    return -1;
}

int f(int x)
{
  if (x != 0 && x != 1)
    return -2;

  else
    return !x;
}

/* Test that x and y are never compared to 0 -- they're always known to be
   0 or 1.  */
/* { dg-final { scan-tree-dump-times "\[xy\]\[^ \]* !=" 0 "vrp1" } } */

/* This one needs more copy propagation that only happens in dom1.  */
/* { dg-final { scan-tree-dump-times "x\[^ \]* & y" 1 "dom1" } } */
/* { dg-final { scan-tree-dump-times "x\[^ \]* & y" 1 "vrp1" { xfail *-*-* } } } */

/* These two are fully simplified by VRP.  */
/* { dg-final { scan-tree-dump-times "x\[^ \]* \[|\] y" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "x\[^ \]* \\^ 1" 1 "vrp1" } } */

/* { dg-final { cleanup-tree-dump "vrp\[0-9\]" } } */
/* { dg-final { cleanup-tree-dump "dom\[0-9\]" } } */
