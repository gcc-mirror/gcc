/* PR rtl-optimization/80501 */

signed char v = 0;

static signed char
foo (int x, int y)
{
  return x << y;
}

__attribute__((noinline, noclone)) int
bar (void)
{
  return foo (v >= 0, __CHAR_BIT__ - 1) >= 1;
}

int
main ()
{
  if (sizeof (int) > sizeof (char) && bar () != 0)
    __builtin_abort ();
  return 0;
}
