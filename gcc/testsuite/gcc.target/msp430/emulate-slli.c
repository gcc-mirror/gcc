/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "mspabi_slli" } } */
/* { dg-final { scan-assembler "rlax" } } */

/* Ensure that HImode shifts with source operand in memory are emulated with a
   rotate instructions.  */

int a;

void
foo (void)
{
  a = a << 4;
}
