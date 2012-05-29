/* Check that the fmac insn is not generated when -mno-fused-madd is specified.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1 -funsafe-math-optimizations -mno-fused-madd" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-not "fmac" } } */

float
test_func (float a, float b, float c, float d, float e, float f)
{
  return a * b + c * d + e * f;
}

