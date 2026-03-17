/* PR tree-optimization/124358 */

[[gnu::noipa]] void
foo (int d)
{
  static int u = 11;
  if (d != u++)
    __builtin_abort ();
}

int
main ()
{
  int a[1][4] = { 11, 12, 13, 14 };
  int (*p)[4] = a;
  for (int i = 0; i < 1; i++)
    for (int j = 0; j < 4; j++)
      foo (*(*(p + i) + j));
}
