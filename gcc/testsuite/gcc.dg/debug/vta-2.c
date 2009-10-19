/* { dg-do compile } */

static int
bar (void *a, unsigned int b, int n)
{
  int c = *(unsigned long *) a % b;
  *(unsigned long *) a = (int) (*(unsigned long *) a) / b;
  return c;
}

int
foo (unsigned long x, int *y, int z)
{
  int level;
  for (level = 0; level < *y; level++)
    {
      bar (&x, z, sizeof (x));
      if (x)
	return *y - 1;
    }
}
