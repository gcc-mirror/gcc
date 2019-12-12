/* { dg-do compile } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2" } */

long foo1 (long a)
{
  long b = a + 1;
  if (b != 0)
    return b;
  return 1;
}

long foo2 (long a)
{
  long b = a + 1;
  if (b < 0)
    return b;
  return 1;
}

long foo3 (long a)
{
  long b = a + 1;
  if (b >= 0)
    return b;
  return 1;
}

/* { dg-final { scan-assembler "movre\t%"  } } */
/* { dg-final { scan-assembler "movrgez\t%"  } } */
/* { dg-final { scan-assembler "movrlz\t%" } } */
