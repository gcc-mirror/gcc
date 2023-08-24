/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O2" } */
/* { dg-final { scan-assembler-not "slli.w" } } */

extern int src1, src2, src3;

int
test (void)
{
  int data1 = src1 + src2;
  int data2 = src1 + src3;

  return data1 > data2 ? data1 : data2;
}
