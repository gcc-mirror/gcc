/* PR rtl-optimization/117095 */

short a, b;
int c, e, z;
long f, g;
int *h = &c, *i;
#ifdef __SIZEOF_INT128__
volatile __int128 j;
#else
volatile long long j;
#endif
char k, l;

char
foo (char n, char o)
{
  return n + o;
}

char
bar (char n, char o)
{
  return o == 0 ? n : n / o;
}

short baz (short n)
{
  return n - a;
}

int
main ()
{
  char *q = &l;
  int **s = &i;
  *s = &z;
  for (e = 0; e <= 5; e++)
    for (g = 1; g <= 5; g++)
      {
	k = foo (9, *i);
	**s = bar (f > 1, (1 && j) ^ k);
      }
  b = baz (q == &l);
  *h = b;
  if (c != 1)
    __builtin_abort ();
}
