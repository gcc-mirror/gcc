/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -mvis3 -msubxc" } */

long neq (long a, long b)
{
  return a != b;
}

long eq (long a, long b)
{
  return a == b;
}

long lt (unsigned long a, unsigned long b)
{
  return a < b;
}

long leq (unsigned long a, unsigned long b)
{
  return a <= b;
}

long geq (unsigned long a, unsigned long b)
{
  return a >= b;
}

long gt (unsigned long a, unsigned long b)
{
  return a > b;
}

/* { dg-final { scan-assembler-times "xor\t%" 2 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 6 } } */
/* { dg-final { scan-assembler-times "addxc\t%" 3 } } */
/* { dg-final { scan-assembler-times "subxc\t%" 3 } } */
/* { dg-final { scan-assembler-not "sra\t%" } } */
/* { dg-final { scan-assembler-not "and\t%" } } */
