/* PR middle-end/120630 */

__attribute__((noipa)) int
foo (const char *x, ...)
{
  return *x;
}

int a, b, c;
unsigned d = 1;

int
main ()
{
  if (a)
    foo ("0");
  int e = -1;
  if (a < 1)
    {
      e = c;
      if (c)
	while (1)
	  ;
    }
  b = (~e + 0UL) / -1;
  if (d > b)
    __builtin_abort ();
  return 0;
}
