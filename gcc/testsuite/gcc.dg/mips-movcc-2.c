/* { dg-do compile { target "mips*-*-*" } } */
/* { dg-options "-O2 -mips4" } */
/* { dg-final { scan-assembler "movz" } } */
/* { dg-final { scan-assembler "movn" } } */
/* { dg-final { scan-assembler "movf" } } */

long
sub4 (long i, long j, long k)
{
  return k ? i : j;
}

long
sub5 (long i, long j, int k)
{
  return !k ? i : j;
}

long
sub6 (long i, long j, float f)
{
  return !f ? i : j;
}
