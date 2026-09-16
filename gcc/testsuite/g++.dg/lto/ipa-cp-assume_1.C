inline bool
baz (int x, int y)
{
  return x < y;
}

int
bar (int x, int y)
{
  [[assume (baz (x, y))]];
  __builtin_printf ("%d %d", x, y);
  return x > y;
}
