/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -msubxc" } */

int eq (long a, long b)
{
  return a == b;
}

int ge (unsigned long a, unsigned long b)
{
  return a >= b;
}

int le (unsigned long a, unsigned long b)
{
  return a <= b;
}

/* { dg-final { scan-assembler "xor\t%" } } */
/* { dg-final { scan-assembler-times "subxc\t%" 3 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 3 } } */
/* { dg-final { scan-assembler-not "sra\t%" } } */
