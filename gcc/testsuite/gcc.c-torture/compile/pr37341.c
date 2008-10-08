/* PR rtl-optimization/37341 */

short int a;
int b;

static inline int
f1 (int x, int y)
{
  if (x < 0 || y < 0 || y >= sizeof (int) * 8 || x > (1 >> y))
    return x;
}

static inline unsigned int
f2 (int x, int y)
{
  if (y <= 0 && x && y < __INT_MAX__ / x)
    return x;
  return x * y;
}

int
f3 (void)
{
  return (signed char) 0xb6;
}

unsigned int
f4 (unsigned int x)
{
  while (1)
    {
      if ((f2 (f3 (), (f1 (a, b)))) < x)
	return 1;
    }
}
