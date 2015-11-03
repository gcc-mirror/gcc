/* Check that the rotcr instruction is generated when shifting the
   negated T bit on non-SH2A.
   On SH2A expect a movrt rotr sequence instead.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */

/* { dg-final { scan-assembler-times "rotcr" 1 { target { ! sh2a } } } } */
/* { dg-final { scan-assembler-times "tst" 1 { target { ! sh2a } } } } */
/* { dg-final { scan-assembler-times "movt" 1 { target { ! sh2a } } } } */

/* { dg-final { scan-assembler-times "movrt" 1 { target { sh2a } } } } */
/* { dg-final { scan-assembler-times "rotr" 1 { target { sh2a } } } } */

int
test_00 (int a, int b)
{
  int r = a != b;
  return r << 31;
}
