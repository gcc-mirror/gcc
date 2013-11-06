/* Testcase to check generation of 'MOVI20S #imm20, Rn'.  */
/* { dg-do assemble }  */
/* { dg-options "-O0" } */
/* { dg-skip-if "" { "sh*-*-*" } "*" "-m2a -m2a-nofpu -m2a-single -m2a-single-only" }  */
/* { dg-final { scan-assembler "movi20s"} }  */

volatile long la;

void
testfun (void)
{
  la = -134217728;
  la = 134217216;
}
