/* { dg-do compile } */
/* { dg-options "-O2 -march=loongarch64 -mabi=lp64d -mdiv32" } */
/* { dg-final { scan-assembler "div\.w" } } */
/* { dg-final { scan-assembler "div\.wu" } } */
/* { dg-final { scan-assembler "mod\.w" } } */
/* { dg-final { scan-assembler "mod\.wu" } } */
/* { dg-final { scan-assembler-not "slli\.w.*,0" } } */

int
divw (long a, long b)
{
  return (int)a / (int)b;
}

unsigned int
divwu (long a, long b)
{
  return (unsigned int)a / (unsigned int)b;
}

int
modw (long a, long b)
{
  return (int)a % (int)b;
}

unsigned int
modwu (long a, long b)
{
  return (unsigned int)a % (unsigned int)b;
}
