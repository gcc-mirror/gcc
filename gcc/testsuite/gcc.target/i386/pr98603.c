/* PR rtl-optimization/98603 */
/* { dg-do compile } */
/* { dg-options "-O0 -w" } */

int
foo (void)
{
  int b, c;
  asm goto ("" : "=r" (b), "=r" (c) : "I" (128) : : lab);	/* { dg-error "impossible constraint in 'asm'" } */
lab:;
}
