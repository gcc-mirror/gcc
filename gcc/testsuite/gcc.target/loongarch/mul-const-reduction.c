/* { dg-do compile } */
/* { dg-options "-O2 -mtune=la464" } */
/* { dg-final { scan-assembler "alsl\.w" } } */
/* { dg-final { scan-assembler "slli\.w" } } */
/* { dg-final { scan-assembler-not "mul\.w" } } */

int
test (int a)
{
  return a * 68;
}
