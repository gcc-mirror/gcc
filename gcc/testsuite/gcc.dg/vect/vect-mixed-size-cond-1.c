/* { dg-do compile } */

int
f (unsigned char *restrict x, short *restrict y)
{
  for (int i = 0; i < 100; ++i)
    {
      unsigned short a = (x[i] + 11) >> 1;
      unsigned short b = (x[i] + 42) >> 2;
      unsigned short cmp = y[i] == 0 ? a : b;
      int res = cmp + 1;
      x[i] = res;
    }
}
