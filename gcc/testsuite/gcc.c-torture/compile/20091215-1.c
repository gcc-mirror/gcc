void bar ();

void
foo (void *x, short y)
{
  bar (x, y + 1);
}

void
bar (x, y)
  void *x;
  char *y;
{
  baz (y);
}
