/* PR target/111408 */

int a, b, c, d;
short e;

int
foo ()
{
  c = a % (sizeof (int) * 8);
  if (b & 1 << c)
    return -1;
  return 0;
}

int
main ()
{
  for (; e != 1; e++)
    {
      int g = foo ();
      if (g + d - 9 + d)
	continue;
      for (;;)
	__builtin_abort ();
    }
}
