/* Verify that the special case (umin (reg const_int 1)) results in the
   expected instruction sequence on SH2A.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" } } */
/* { dg-final { scan-assembler-times "tst" 1 } } */
/* { dg-final { scan-assembler-times "movrt" 1 } } */

unsigned int
test_00 (unsigned int a)
{
  /* 1x tst
     1x movrt  */
  return a > 1 ? 1 : a;
}
