/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "mspabi_srai" } } */
/* { dg-final { scan-assembler "rrax" } } */

/* Ensure that HImode shifts with source operand in memory are emulated with a
   rotate instructions.  */

int a;

void
foo (void)
{
  a = a >> 4;
}
