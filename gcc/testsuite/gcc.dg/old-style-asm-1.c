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

/* { dg-final { scan-assembler "L2" } } */
