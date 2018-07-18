/* { dg-do compile } */

int foo (int *p)
{
  int i;
  for (i = 0; i < 5; i++)
    {
      if (p[i] & 1)
        return i;
    }
  return -1;
}
