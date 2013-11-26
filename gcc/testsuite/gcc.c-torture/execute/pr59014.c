/* PR tree-optimization/59014 */

int a = 2, b, c, d;

int
foo ()
{
  for (;; c++)
    if ((b > 0) | (a & 1))
      ;
    else
      {
	d = a;
	return 0;
      }
}

int
main ()
{
  foo ();
  if (d != 2)
    __builtin_abort ();
  return 0;
}
