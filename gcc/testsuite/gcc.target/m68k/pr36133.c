/* pr36133.c

   This test ensures that conditional branches can use the condition codes
   written by shift instructions, without the need for an extra TST.  */

/* { dg-do compile }  */
/* { dg-options "-O2" }  */
/* { dg-final { scan-assembler-not "tst" } } */

void
f (unsigned int a)
{
  if (a >> 4)
    asm volatile ("nop");
  asm volatile ("nop");
}
