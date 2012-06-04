/* PR rtl-optimization/53519 */

int a, b, c, d, e;

short int
foo (short int x)
{
  return a == 0 ? x : 0;
}

short int
bar (int x, int y)
{
  return x + y;
}

void
baz (void)
{
  if (!e)
    {
      int f = foo (65535 ^ b);
      if (bar (!6L <= ~f, ~e) == c)
	d = 0;
    }
}
