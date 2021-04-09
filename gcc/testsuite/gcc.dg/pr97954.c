/* PR rtl-optimization/97954 */
/* { dg-do compile { target lra } } */
/* { dg-options "-O2" } */

int
foo (void)
{
  int x;
 lab:
  asm goto ("": "=r" (x) : : : lab);
  return x;
}
