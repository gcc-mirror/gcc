/* PR target/11877 */
/* { dg-do compile } */
/* { dg-options "-Os" } */

void
foo (int *p)
{
  p[0] = 0;
  p[7] = 0;
  p[23] = 0;
  p[41] = 0;
  p[48] = 0;
  p[59] = 0;
  p[69] = 0;
  p[78] = 0;
  p[83] = 0;
  p[89] = 0;
  p[98] = 0;
  p[121] = 0;
  p[132] = 0;
  p[143] = 0;
  p[154] = 0;
}

/* { dg-final { scan-assembler-times "xorl\[ \t\]" 1 } } */
/* { dg-final { scan-assembler-not "\\\$0," } } */
