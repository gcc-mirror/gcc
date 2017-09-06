/* PR tree-optimization/81588 */
/* { dg-do compile { target { ! { { logical_op_short_circuit && { ! "mips*-*-* avr-*-* s390*-*-*" } } || { m68k*-*-* bfin*-*-* v850*-*-* moxie*-*-* m32c*-*-* fr30*-*-* mcore*-*-* xtensa*-*-* hppa*-*-* } } } } } */
/* { dg-options "-O2 -fdump-tree-reassoc1-details" } */
/* { dg-additional-options "-mbranch-cost=2" { target mips*-*-* avr-*-* s390*-*-* i?86-*-* x86_64-*-* } } */

extern long long int a, c;
extern unsigned short b;

/* { dg-final { scan-tree-dump-times "Optimizing range test \[^\n\r]* and comparison" 1 "reassoc1" } } */

__attribute__((noinline, noclone)) void
foo (void)
{
  if ((b > a) != (1 + (a < 0)))
    c = 0;
}
