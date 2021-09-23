/* PR rtl-optimization/100590 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-dce -w" } */

int
foo (void)
{
  int x;
  asm goto ("" : "+r" (x) : : : lab);
  return 0;
 lab:
  return 1;
}
