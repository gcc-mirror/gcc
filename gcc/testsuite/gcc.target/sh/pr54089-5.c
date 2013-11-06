/* Check that the movrt rotr instruction sequence is generated when shifting
   the negated T bit on SH2A.  */
/* { dg-do compile }  */
/* { dg-options "-O1" } */
/* { dg-skip-if "" { "sh*-*-*" } { "*" } { "-m2a*" } } */
/* { dg-final { scan-assembler-times "movrt" 1 } } */
/* { dg-final { scan-assembler-times "rotr" 1 } } */

int
test_00 (int a, int b)
{
  int r = a != b;
  return r << 31;
}
