/* PR inline-asm/8832 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

/* Verify that GCC doesn't optimize
   old style asm instructions.  */

void foo(int v)
{
  if (v)
    asm ("dummy1");

  asm ("dummy2");

  if (v)
    asm ("dummy3");
}

/* The purpose of the test below is to check that there are two branches
   in the generated code, supposedly corresponding to the if-statements.
   Warning: this is fragile and assumes that the generated labels for the
   branches contain letter "L".  That assumption is generally invalid,
   because for example it depends on the target macro
   ASM_GENERATE_INTERNAL_LABEL to generate such a name (as with the default
   definition).  */
/* { dg-final { scan-assembler-times "L" 4 } } */
