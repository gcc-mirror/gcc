/* PR target/122534 */
/* { dg-do compile } */
/* { dg-options "-O2" } */

int test (unsigned long p[6], int index)
{
  __SIZE_TYPE__ i;

  for (i = 0; i < 6; i++)
    if (p[i] & (1UL << index))
      return i;
  return 0;
}

/* { dg-final { scan-assembler-not "and" } } */
