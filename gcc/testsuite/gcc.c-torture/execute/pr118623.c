/* PR target/118623 */

static int
foo (int x, int y)
{
  int a = 1 << x;
  if (y & a)
    return 0;
  return 5;
}

__attribute__((noipa)) void
bar (int x)
{
  if (((foo (x - 50, x) + x + x) & 1) == 0)
    __builtin_abort ();
}

int
main ()
{
  bar (63);
}
