/* PR rtl-optimization/57875 */

extern void abort (void);
int a[1], b, c, d, f, i;
char e[1];

int
main ()
{
  for (; i < 1; i++)
    if (!d)
      {
	if (!c)
	  f = 2;
	e[0] &= f ^= 0;
      }
  b = a[e[0] >> 1 & 1];
  if (b != 0)
    abort ();
  return 0;
}
