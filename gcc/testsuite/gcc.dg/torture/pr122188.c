/* PR c/122188 */
/* { dg-do run } */

int
foo (unsigned x, int y)
{
  unsigned a = x;
  int b = y;
  int ret = __builtin_ctzg (x++, y++);
  if (x != a + 1 || y != b + 1)
    __builtin_abort ();
  return ret;
}

int
bar (unsigned x, int y)
{
  unsigned a = x;
  int b = y;
  int ret = __builtin_clzg (x++, y++);
  if (x != a + 1 || y != b + 1)
    __builtin_abort ();
  return ret;
}

int
main ()
{
  if (foo (0, 42) != 42 || foo (1, 5) != 0 || foo (4, 17) != 2)
    __builtin_abort ();
  if (bar (0, 42) != 42 || bar (~0U, 5) != 0 || bar (~0U >> 4, 17) != 4)
    __builtin_abort ();
}
