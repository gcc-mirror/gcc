/* PR target/94567 */

volatile int a = 1, b;
short c, d = 4, f = 2, g;
unsigned short e = 53736;

int
foo (int i, int j)
{
  return i && j ? 0 : i + j;
}

int
main ()
{
  for (; a; a = 0)
    {
      unsigned short k = e;
      g = k >> 3;
      if (foo (g < (f || c), b))
	d = 0;
    }
  if (d != 4)
    __builtin_abort ();
  return 0;
}
