/* { dg-do compile } */
/* { dg-options "-O0 -fno-pic" } */

/* Verify that __attribute__((naked)) produces a naked function 
   that does not use ret to return but traps at the end.  */
void
__attribute__((naked))
foo (void)
{
  __asm__ ("# naked");
}
/* { dg-final { scan-assembler "# naked" } } */
/* { dg-final { scan-assembler "ud2" } } */
/* { dg-final { scan-assembler-not "ret" } } */
