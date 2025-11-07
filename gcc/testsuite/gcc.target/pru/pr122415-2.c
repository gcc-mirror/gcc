/* { dg-options "-O1" } */

void test(void)
{
  (*(unsigned *)0x11223364) = 1;
  (*(unsigned *)0x11223344) = 1;
}
/* { dg-final { scan-assembler "sbbo\\tr\[0-9.bw\]*, r\[0-9\]*, 100, 4" } } */
/* { dg-final { scan-assembler "sbbo\\tr\[0-9.bw\]*, r\[0-9\]*, 68, 4" } } */
