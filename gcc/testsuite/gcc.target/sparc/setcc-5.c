/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -mvis3" } */

long neq (long a, long b)
{
  return a != b;
}

long lt (unsigned long a, unsigned long b)
{
  return a < b;
}

long gt (unsigned long a, unsigned long b)
{
  return a > b;
}

/* { dg-final { scan-assembler-times "xor\t%" 1 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 3 } } */
/* { dg-final { scan-assembler-times "addxc\t%" 3 } } */
/* { dg-final { scan-assembler-not "sra\t%" } } */
/* { dg-final { scan-assembler-not "and\t%" } } */
