/* { dg-do compile } */

void
foo (unsigned long long *x, unsigned long long *y, int z)
{
  int i;
  for (i = 0; i < 1024; i++)
    x[i] = (y[i] >> z) | (y[i] << (-z & (__SIZEOF_LONG_LONG__ * __CHAR_BIT__ - 1)));
}
