/* { dg-do compile } */
/* { dg-options "-O2 -mno-volatile-cache" } */

volatile int i;
void f (void)
{
  i = 0;
}
/* { dg-final { scan-assembler "st\.di" } } */
