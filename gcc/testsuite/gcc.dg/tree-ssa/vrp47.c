/* Skip on MIPS/ARC, where LOGICAL_OP_NON_SHORT_CIRCUIT inhibits the setcc
   optimizations that expose the VRP opportunity.  */
/* Skip on S/390 and avr.  Lower values in BRANCH_COST lead to two conditional
   jumps when evaluating an && condition.  VRP is not able to optimize
   this.  */
/* { dg-do compile { target { ! "mips*-*-* arc*-*-* s390*-*-*  avr-*-* mn10300-*-*" } } } */
/* { dg-options "-O2 -fdump-tree-vrp1 -fdump-tree-dom1 -fdump-tree-vrp2" } */
/* { dg-additional-options "-march=i586" { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* Skip on ARM Cortex-M, where LOGICAL_OP_NON_SHORT_CIRCUIT is set to false,
   leading to two conditional jumps when evaluating an && condition.  VRP is
   not able to optimize this.  */
/* { dg-skip-if "" { arm_cortex_m } } */

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

/* These two are fully simplified by VRP1.  */
/* { dg-final { scan-tree-dump-times "x\[^ \]* \[|\] y" 1 "vrp1" } } */
/* { dg-final { scan-tree-dump-times "x\[^ \]* \\^ 1" 1 "vrp1" } } */

/* VRP2 gets rid of the remaining & 1 operations, x and y are always
   either 0 or 1.  */
/* { dg-final { scan-tree-dump-times " & 1;" 0 "vrp2" } } */

/* { dg-final { cleanup-tree-dump "vrp1" } } */
/* { dg-final { cleanup-tree-dump "dom1" } } */
/* { dg-final { cleanup-tree-dump "vrp2" } } */
