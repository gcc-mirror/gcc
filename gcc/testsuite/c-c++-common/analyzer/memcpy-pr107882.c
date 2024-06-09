void
foo (int *x, int y)
{
  int *a = x, *b = (int *) &a;

  __builtin_memcpy (b + 1, x, y);
  foo (a, 0);
}
