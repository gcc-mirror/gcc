/* { dg-do compile } */
/* { dg-options "-O2 -mvolatile-cache" } */

volatile int i;
void f (void)
{
  i = 0;
}
/* { dg-final { scan-assembler-not "st\.di" } } */
