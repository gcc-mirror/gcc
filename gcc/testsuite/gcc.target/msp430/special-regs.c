/* { dg-do compile } */

int foo (void)
{
  register int pc __asm__("R0");
  register int sp __asm__("R1");
  register int cg1 __asm__("R2"); /* { dg-error "the register specified for 'cg1' is not general enough" } */
  register int cg2 __asm__("R3"); /* { dg-error "the register specified for 'cg2' is not general enough" } */

  asm("" : "=r"(pc));
  asm("" : "=r"(sp));
  asm("" : "=r"(cg1));
  asm("" : "=r"(cg2));

  return pc + sp + cg1 + cg2;
}
