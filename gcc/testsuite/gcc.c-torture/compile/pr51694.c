void
foo (x, fn)
  void (*fn) ();
{
  int a = baz ((void *) 0, x);
  (*fn) (x, 0);
}

void
bar (void)
{
  void *x = 0;
  foo (x);
}
