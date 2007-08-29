/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "j(ra|mp)\[ \t\]*interrupt_sibcall" } } */
/* { dg-final { scan-assembler "j(b|)sr\[ \t\]*interrupt_call" } } */
/* { dg-final { scan-assembler "j(ra|mp)\[ \t\]*normal_sibcall" } } */

void normal_sibcall (void);
void interrupt_call (void);
void __attribute ((interrupt)) interrupt_sibcall (void);

void normal (void)
{
  normal_sibcall ();
}

void __attribute ((interrupt)) interrupt (void)
{
  interrupt_call ();
}

void __attribute ((interrupt)) interrupt_2 (void)
{
  interrupt_sibcall ();
}
