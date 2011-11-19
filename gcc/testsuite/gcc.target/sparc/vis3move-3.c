/* { dg-do compile } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O1 -mvis3" } */

float fnegs (float a)
{
  return -a;
}

double fnegd (double a)
{
  return -a;
}

float fmuls (float a, float b)
{
  return a * b;
}

double fmuld (double a, double b)
{
  return a * b;
}

double fsmuld (float a, float b)
{
  return (double)a * (double)b;
}

double fnsmuld (float a, float b)
{
  return -((double)a * (double)b);
}

/* { dg-final { scan-assembler-times "movwtos\t%" 13 } } */
/* { dg-final { scan-assembler "fnegs\t%" } } */
/* { dg-final { scan-assembler "fnegd\t%" } } */
/* { dg-final { scan-assembler "fmuls\t%" } } */
/* { dg-final { scan-assembler "fmuld\t%" } } */
/* { dg-final { scan-assembler "fsmuld\t%" } } */
/* { dg-final { scan-assembler "fnsmuld\t%" } } */
