/* Verify the return instruction is mret.  */
/* { dg-do compile } */
/* { dg-options "-O" } */
void __attribute__ ((interrupt ("user")))
foo (void)
{
}
/* { dg-final { scan-assembler "uret" } } */
