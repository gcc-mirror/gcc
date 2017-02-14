/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -msubxc" } */

long foo2 (long a, long i)
{
  return a - (i != 0);
}

long foo4 (long a, long b, long i)
{
  return a - b - (i != 0);
}

/* { dg-final { scan-assembler-times "subxc\t%" 2 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 2 } } */
/* { dg-final { scan-assembler-not "sub\t%" } } */
