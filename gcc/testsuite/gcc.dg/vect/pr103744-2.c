/* { dg-do compile } */

int
f1 (int *restrict x, unsigned short *restrict y)
{
  int res = 0;
  for (int i = 0; i < 100; i += 2)
    {
      unsigned short i1 = y[i + 0] + 1;
      unsigned short i2 = y[i + 1] + 2;
      res += x[i1];
      res += x[i2];
    }
  return res;
}

void
f2 (int *restrict x, unsigned short *restrict y)
{
  int res1 = 0;
  int res2 = 0;
  for (int i = 0; i < 100; i += 2)
    {
      unsigned short i1 = y[i + 0] + 1;
      unsigned short i2 = y[i + 1] + 2;
      res1 += x[i1];
      res2 += x[i2];
    }
  x[0] = res1;
  x[1] = res2;
}
