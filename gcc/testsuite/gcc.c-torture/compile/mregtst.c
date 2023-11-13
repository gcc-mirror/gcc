/* { dg-additional-options "-std=gnu89" } */

foo (a, p)
     int *p;
{
  int old, new, i;

  old = 0;
  for (i = 1; i < 100; i++)
    {
      new = p[i];
      if (new < old)
	a++;
      old = new;
      if (old == 0)
	return 0;
    }
  return a;
}
