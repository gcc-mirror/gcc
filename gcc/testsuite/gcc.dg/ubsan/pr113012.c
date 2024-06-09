/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

int *
foo (int x, int y, int z, int w)
{
  int *p = __builtin_malloc (z * sizeof (int));
  int *q = p - 1;
  while (--x > 0)
    {
      if (w + 1 > y)
	q = p - 1;
      ++*q;
      ++q;
    }
  return p;
}
