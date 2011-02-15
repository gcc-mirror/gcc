/* PR middle-end/47581 */
/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -mpreferred-stack-boundary=4 -mincoming-stack-boundary=4" } */
/* { dg-final { scan-assembler-not "(sub|add)l\[\\t \]*\\$\[0-9\]*,\[\\t \]*%\[re\]?sp" } } */

unsigned
foo (unsigned a, unsigned b)
{
  return ((unsigned long long) a * (unsigned long long) b) >> 32;
}
