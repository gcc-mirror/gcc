/* { dg-do compile } */

#define N 1024

void
f (unsigned int *x1, unsigned int *x2, unsigned short *y, unsigned char z)
{
  unsigned short zu = z;
  for (int i = 0; i < N; ++i)
    {
      unsigned short yi = y[i];
      x1[i] = x1[i] > 10 ? yi * zu : x1[i] + 1;
      x2[i] += 1;
    }
}
