/* PR rtl-optimization/98603 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

int
foo (void)
{
  int b, c;
  asm goto ("" : "=R" (b), "=r" (c) : : : lab);	/* { dg-error "impossible constraint in 'asm'" } */
lab:;
}
