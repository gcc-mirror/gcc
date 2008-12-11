/* { dg-options "-O3 -floop-block" } */

void test_sort()
{
  char *base;
  register char c, *i, *hi;

  for (i = base; i < hi; i++)
    *i++ = c;
}

