/* { dg-do compile } */
/* { dg-options "-Os" } */

void test(unsigned int count, double array[])
{
  unsigned int i;
  for (i = 0; i < count; ++i)
    array[i] = 8.988474246316506e+307;
}

/* { dg-final { scan-assembler-not "l32r" } } */
