/* { dg-do compile } */
/* { dg-options "-O -mabi=32 -mfp64" } */
/* { dg-final { scan-assembler "mthc1" } } */
/* { dg-final { scan-assembler "mfhc1" } } */

NOMIPS16 double func1 (long long a)
{
  return a;
}

NOMIPS16 long long func2 (double b)
{
  return b;
}
