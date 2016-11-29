/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O1 -mvis3" } */

long foo1 (long a, long i)
{
  return a + (i != 0);
}

long foo3 (long a, long b, long i)
{
  return a + b + (i != 0);
}

/* { dg-final { scan-assembler-times "addxc\t%" 2 } } */
/* { dg-final { scan-assembler-times "cmp\t%" 2 } } */
/* { dg-final { scan-assembler-not "add\t%" } } */
