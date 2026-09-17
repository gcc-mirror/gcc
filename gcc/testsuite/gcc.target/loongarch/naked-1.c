/* { dg-do compile } */
/* { dg-options "-O0" } */

/* Verify that __attribute__((naked)) produces a function with neither
   a prologue nor an epilogue.  */
void
__attribute__((naked))
foo (void)
{
  __asm__ ("# naked");
}

/* { dg-final { scan-assembler "# naked" } } */
/* { dg-final { scan-assembler-not "(?n)^\\s*jr\\t\\\$r1" } } */
