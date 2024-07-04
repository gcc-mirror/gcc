/* { dg-options "-O3" } */

#pragma GCC target "+nosve"

int test(unsigned array[4][4]);

int foo(unsigned short *a, unsigned long n)
{
  unsigned array[4][4];

  for (unsigned i = 0; i < 4; i++, a += 4)
    {
      array[i][0] = a[0] << 6;
      array[i][1] = a[1] << 6;
      array[i][2] = a[2] << 6;
      array[i][3] = a[3] << 6;
    }

  return test(array);
}

/* { dg-final { scan-assembler-times {\tushll\t} 2 } } */
/* { dg-final { scan-assembler-times {\tushll2\t} 2 } } */
