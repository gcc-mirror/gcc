/* Testcase to check generation of 'MOVI20S #imm20, Rn'.  */
/* { dg-do compile { target { sh2a } } }  */
/* { dg-options "-O0" } */
/* { dg-final { scan-assembler "movi20s"} }  */

volatile long la;

void
testfun (void)
{
  la = -134217728;
  la = 134217216;
}
