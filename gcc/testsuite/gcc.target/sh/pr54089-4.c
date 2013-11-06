/* Check that the rotcr instruction is generated when shifting the
   negated T bit on non-SH2A.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "-m5*" "-m2a*" } { "" } }  */
/* { dg-final { scan-assembler-times "rotcr" 1 } } */
/* { dg-final { scan-assembler-times "tst" 1 } } */
/* { dg-final { scan-assembler-times "movt" 1 } } */

int
test_00 (int a, int b)
{
  int r = a != b;
  return r << 31;
}
