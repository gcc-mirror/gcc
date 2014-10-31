/* PR rtl-optimization/63659 */

int a, b, c, *d = &b, g, h, i;
unsigned char e;
char f;

int
main ()
{
  while (a)
    {
      for (a = 0; a; a++)
	for (; c; c++)
	  ;
      if (i)
	break;
    }

  char j = c, k = -1, l;
  l = g = j >> h;
  f = l == 0 ? k : k % l;
  e = 0 ? 0 : f;
  *d = e;

  if (b != 255)
    __builtin_abort ();

  return 0;
}
