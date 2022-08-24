/* { dg-do compile } */

int
foo (unsigned long int x, int y, int z)
{
  int ret = 0;

  while (y < 1)
    {
      x *= 2;
      ret = x == z;
      z = y;
      ++y;
    }

  return ret;
}
