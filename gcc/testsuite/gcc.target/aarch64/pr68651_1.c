/* { dg-do compile } */
/* { dg-options "-O2 -mcpu=cortex-a53" } */

int
foo (int x)
{
  return (x * 2) & 65535;
}
/* { dg-final { scan-assembler "ubfiz\tw\[0-9\]*, w\[0-9\]*.*\n" } } */

int
bar (int x, int y)
{
  return (x * 2) | y;
}
/* { dg-final { scan-assembler "orr\tw\[0-9\]*, w\[0-9\]*, w\[0-9\]*, lsl 1.*\n" } } */
