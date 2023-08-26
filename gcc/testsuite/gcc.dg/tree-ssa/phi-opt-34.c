/* { dg-do compile } */
/* Disable early phiopt1 as  early ccp1 does not export non-zero bits
   so at the point of phiopt1, we don't know that a is [0,1] range */
/* { dg-options "-O1 -fdisable-tree-phiopt1 -fdump-tree-phiopt2-folding" } */

unsigned f(unsigned a)
{
  a &= 1;
  if (a > 0)
    return a;
  return 1;
}
/* PHIOPT2 should be able to change this into just return 1;
   (which was `MAX<a, 1>` or `a | 1` but since a is known to be a
   range of [0,1], it should be folded into 1)
   And not fold it into `(a == 0) | a`. */
/* { dg-final { scan-tree-dump-not " == " "phiopt2" } } */
/* { dg-final { scan-tree-dump-not " if " "phiopt2" } } */
/* { dg-final { scan-tree-dump-not "Folded into the sequence:" "phiopt2" } } */
/* { dg-final { scan-tree-dump "return 1;" "phiopt2" } } */
/* We want to make sure that phiopt2 is happening and not some other pass
   before it does the transformation. */
/* { dg-final { scan-tree-dump "Removing basic block" "phiopt2" } } */
