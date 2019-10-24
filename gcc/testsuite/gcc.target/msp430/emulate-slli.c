/* { dg-do compile } */
/* { dg-skip-if "" { *-*-* } { "-mcpu=msp430" } { "" } } */
/* { dg-options "-Os" } */
/* { dg-final { scan-assembler-not "mspabi_slli" } } */
/* { dg-final { scan-assembler "RLAM.W\t#4" } } */
/* { dg-final { scan-assembler "RPT\t#5 \{ RLAX.W" } } */

/* Ensure that HImode shifts with source operand in memory are emulated with a
   rotate instructions.  */

int a;
int b;

void
foo (void)
{
  a = a << 4;
  b = b << 5;
}
