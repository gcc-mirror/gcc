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
   Warning: this is fragile and assumes that one of the generated labels
   for the branches matches the string "L2", or as with
   mmix-knuth-mmixware, "L:2".  That assumption is generally invalid,
   because for example it depends on the target macro
   ASM_GENERATE_INTERNAL_LABEL to generate a name matching this regexp (as
   with the default definition).  */
/* { dg-final { scan-assembler "L(:|\\\$0*)?2" } } */
