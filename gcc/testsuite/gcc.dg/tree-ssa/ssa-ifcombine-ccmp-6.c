/* { dg-do compile { target { ! "m68k*-*-* mmix*-*-* mep*-*-* bfin*-*-* v850*-*-* picochip*-*-* moxie*-*-* cris*-*-* m32c*-*-* fr30*-*-* mcore*-*-* powerpc*-*-* xtensa*-*-*"} } } */

/* { dg-options "-O2 -g -fdump-tree-optimized" } */
/* { dg-additional-options "-mbranch-cost=2" { target avr-*-* } } */

int t (int a, int b, int c)
{
  if (a > 0 || b > 0 || c > 0)
      return 0;
  return 1;
}
/* { dg-final { scan-tree-dump-times "\\|" 2 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
