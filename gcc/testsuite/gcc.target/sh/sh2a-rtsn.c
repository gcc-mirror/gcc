/* Testcase to check generation of a SH2A specific instruction for
   'RTS/N'.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-options "-O0" }  */
/* { dg-final { scan-assembler "rts/n"} }  */

void
bar (void)
{
}
