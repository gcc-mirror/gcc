/* { dg-do compile } */
/* { dg-options "-O0" } */
/* Check that function arguments aren't assigned and copied to stack slots
   in naked functions.  This usually happens at -O0 (presumably for
   better debugging), but is highly undesirable if we haven't created
   a stack frame.  */
void __attribute__((naked))
foo(int n)
{
  __asm__ volatile ("frob r0\n");
}
/* { dg-final { scan-assembler "\tfrob r0" } } */
/* { dg-final { scan-assembler-not "\tstr" } } */
