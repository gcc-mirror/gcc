extern void baz (int, int, int);

int j;

int
bar (void)
{
  int n = 0, *np = &n;
  if (j)
    baz (0, 0, 0);
  if (j)
    baz (0, 0, 0);
  return n;
}

void
foo (void)
{
  bar ();
  bar ();
}

