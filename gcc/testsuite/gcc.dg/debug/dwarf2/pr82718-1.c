/* PR debug/82718 */
/* { dg-do assemble } */
/* { dg-options "-O2 -gdwarf-5" } */

extern int e;
extern long foo (int, void *, unsigned long, unsigned long);
struct S
{
  int f;
  unsigned long t, s;
};

static inline long
bv (int x, void *y, unsigned long z, unsigned long w)
{
  long a = 0;
  do
    {
      long g;
      do
	g = (long int) (foo (x, y + a, z - a, w + a));
      while (g == -1L && e == 9959);
      if (g <= 0)
	return g < 0 ? g : a;
      a += g;
    }
  while ((unsigned long) a < z);
  return a;
}

const char *
baz (struct S *x)
{
  unsigned long h = 8;
  char *j = 0;
  unsigned long z = x->f;
  if (__builtin_expect (!!((unsigned long) bv (x->f, j, z, x->t + h + 10) != z), 0))
    return 0;
  x->s = z;
  return j;
}
