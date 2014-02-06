/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-require-effective-target dfp } */
/* { dg-options "-O2" } */

void foo (void)
{
  register float __attribute__ ((mode(SD))) r31 __asm__ ("r31");
  register float __attribute__ ((mode(SD))) fr1 __asm__ ("fr1");

  __asm__ ("#" : "=d" (fr1));
  r31 = fr1;
  __asm__ ("#" : : "r" (r31));
}
