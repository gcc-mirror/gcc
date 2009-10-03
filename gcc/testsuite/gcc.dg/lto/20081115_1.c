extern inline int
e_inline_baz (void)
{
  return 2 + 1;
}

int
foo (void)
{
  return e_inline_baz ();
}
