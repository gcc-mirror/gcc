/* { dg-do compile } */
/* { dg-mips-options "-O -mabi=32 -mfp64" } */
/* { dg-final { scan-assembler "mthc1" } } */
/* { dg-final { scan-assembler "mfhc1" } } */

double func1 (long long a)
{
  return a;
}

long long func2 (double b)
{
  return b;
}
