/* { dg-do compile } */
/* { dg-options "-O0" } */

/* Verify that __attribute__((naked)) produces a naked function 
   that does not construct a frame.  */
void
__attribute__((naked))
foo (void)
{
  __asm__ ("# naked");
}
/* { dg-final { scan-assembler "# naked" } } */
/* { dg-final { scan-assembler-not "(?n)^\\s*push" } } */
/* { dg-final { scan-assembler-not "(?n)^\\s*pop" } } */
