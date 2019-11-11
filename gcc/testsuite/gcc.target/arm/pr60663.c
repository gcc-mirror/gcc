/* PR rtl-optimization/60663 */
/* { dg-do compile } */
/* { dg-skip-if "-mpure-code supports M-profile only" { *-*-* } { "-mpure-code" } } */
/* { dg-options "-O2 -march=armv7-a" } */

int
foo (void)
{
  unsigned i, j;
  asm ("%0 %1" : "=r" (i), "=r" (j));
  return i;
}
