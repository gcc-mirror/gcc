/* { dg-do compile { target { ! { { logical_op_short_circuit && { ! avr-*-* } } || { m68k*-*-* mmix*-*-* bfin*-*-* v850*-*-* moxie*-*-* cris*-*-* m32c*-*-* fr30*-*-* mcore*-*-* powerpc*-*-* xtensa*-*-* arc*-*-* mips*-*-* } } } } } */

/* { dg-options "-O2 -g -fdump-tree-optimized" } */
/* { dg-additional-options "-mbranch-cost=2" { target mips*-*-* avr-*-* s390*-*-* i?86-*-* x86_64-*-* } } */

int t (int a, int b, int c)
{
  if (a > 0 || b > 0 || c > 0)
      return 0;
  return 1;
}
/* { dg-final { scan-tree-dump-times "\\|" 2 "optimized" } } */
