/* { dg-do compile } */

int
f (int *y)
{
  int res = 0;
  for (int i = 0; i < 100; ++i)
    res = (y[i] & 1) == 0 && (y[i] < 10) ? res : 1;
  return res;
}
