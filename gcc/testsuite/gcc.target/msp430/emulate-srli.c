/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "mspabi_srli" } } */
/* { dg-final { scan-assembler "rrum" } } */

/* Ensure that HImode shifts with source operand in memory are emulated with a
   rotate instructions.  */

unsigned int a;

void
foo (void)
{
  a = a >> 4;
}
