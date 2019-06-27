/* PR middle-end/51768 */
/* { dg-do compile } */
/* { dg-options "" } */

void
foo (void)
{
  asm goto ("" : : : : lab, lab, lab2, lab);	/* { dg-error "duplicate 'asm' operand name" } */
lab:;
lab2:;
}

void
bar (void)
{
  asm goto ("" : : [lab] "i" (0) : : lab);	/* { dg-error "duplicate 'asm' operand name" } */
lab:;
}

void
baz (void)
{
  int x;
  asm ("" : [lab] "=r" (x) : [lab] "r" (x));	/* { dg-error "duplicate 'asm' operand name" } */
}
