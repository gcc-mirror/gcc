/* Verify that the fmac insn is used for the expression 'a * b + a' and
   'a * a + a'.
   This assumes that the default compiler setting is -ffp-contract=fast.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-times "fmac" 2 } } */

float
test_00 (float a, float b)
{
  return a * b + a;
}

float
test_01 (float a)
{
  return a * a + a;
}
