/* Check that no unnecessary sign or zero extension insn is generated after
   a negc or movrt insn that stores the inverted T bit in a reg.  */
/* { dg-do compile { target "sh*-*-*" } } */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m1" "-m2" "-m3" "-m4al" "*nofpu" "-m4-340*" "-m4-400*" "-m4-500*" "-m5*" } { "" } }  */
/* { dg-final { scan-assembler-not "extu|exts" } } */

float
test_00 (float q[4], float m[9])
{
  float s0 = m[0] + m[1];
  float s1 = m[0] - m[1];

  return q[s0 > s1 ?  0 : 1];
}
