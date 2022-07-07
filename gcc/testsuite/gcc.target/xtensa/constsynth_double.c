/* { dg-do compile } */
/* { dg-options "-Os" } */

void test(unsigned int count, double array[])
{
  unsigned int i;
  for (i = 0; i < count; ++i)
    array[i] = 1.0;
}

/* { dg-final { scan-assembler-not "l32r" } } */
