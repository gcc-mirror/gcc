/* { dg-options "-O3 -mtune=ampere1" } */

long
foo (long a[])
{
  return a[0] + a[1];
}

/* We should see two ldrs instead of one ldp. */
/* { dg-final { scan-assembler {\tldr\t} } } */
/* { dg-final { scan-assembler-not {\tldp\t} } } */
