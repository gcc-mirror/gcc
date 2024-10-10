/* PR middle-end/97971 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int
foo (void)
{
  register _Complex long long a asm ("rax");
  register int b asm ("rdx");
  asm ("# %0 %1" : "=&r" (a), "=r" (b));	/* { dg-error "inconsistent operand constraints in an 'asm'" } */
  return a;
}
