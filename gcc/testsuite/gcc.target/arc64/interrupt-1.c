/* Verify the return instruction is iret.  */
/* { dg-do compile } */
/* { dg-options "-O" } */
void __attribute__ ((interrupt ("ilink")))
foo (void)
{
}
/* { dg-final { scan-assembler "rtie" } } */
