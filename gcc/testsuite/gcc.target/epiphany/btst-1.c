/* { dg-do compile } */
/* { dg-options "-O2 -fno-common" } */
/* { dg-final { scan-assembler-not "movt" } } */
/* { dg-final { scan-assembler-not "and" } } */
/* { dg-final { scan-assembler "lsl" } } */

int
tst (int i)
{
  return (i & (1<<21)) ? 6 : 9;
}
