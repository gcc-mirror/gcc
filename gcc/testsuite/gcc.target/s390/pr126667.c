/* PR target/126667 */
/* { dg-do compile } */
/* { dg-options "-O2 -march=z14" } */

unsigned long
foo (unsigned long x, unsigned long y, unsigned long *p)
{
  return __builtin_subcl (x, y, 1UL, p);
}
