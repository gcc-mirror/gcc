/* { dg-do compile { target { ! "m68k*-*-* mmix*-*-* mep*-*-* bfin*-*-* v850*-*-* picochip*-*-* moxie*-*-* cris*-*-* m32c*-*-* fr30*-*-* mcore*-*-* powerpc*-*-* xtensa*-*-*"} } } */

/* { dg-options "-O2 -g -fdump-tree-optimized" } */
/* { dg-additional-options "-mbranch-cost=2" { target avr-*-* } } */

int t (int a, int b)
{
  if (a > 0)
    goto L1;
  if (b > 0)
    goto L1;
  return 0;
L1:
  return 1;
}
/* { dg-final { scan-tree-dump "\|" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
