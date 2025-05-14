/* PR tree-optimization/118953 */
/* { dg-do run { target int32plus } } */

int a, d;
long long b, c;

int
foo (int f, int g, unsigned long long h, long long j)
{
  unsigned long long i = 0;
  if (g)
    switch (f)
      {
      case 8:
	i = b;
	break;
      case 6:
	i = c;
	break;
      }
  else
    switch (f)
      {
      case 8:
	i = h;
	break;
      case 24:
      case 32:
	i = j;
	break;
      }
  return i;
}

int
main ()
{
  int k = a * (409628 - 28);
  d = foo (k - 1048524, 0, k - 1048487, k - 1048531ULL);
  if (d)
    __builtin_abort ();
}
