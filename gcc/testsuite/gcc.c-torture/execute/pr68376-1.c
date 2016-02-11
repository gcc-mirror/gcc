/* PR rtl-optimization/68376 */

int a, b, c = 1;
signed char d;

int
main ()
{
  for (; a < 1; a++)
    for (; b < 1; b++)
      {
	signed char e = ~d;
	if (d < 1)
	  e = d;
	d = e;
	if (!c)
	  __builtin_abort ();
      }

  if (d != 0)
    __builtin_abort ();

  return 0;
}
