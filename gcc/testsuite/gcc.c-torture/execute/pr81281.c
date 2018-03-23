/* PR sanitizer/81281 */

void
foo (unsigned p, unsigned a, unsigned b)
{
  unsigned q = p + 7;
  if (a - (1U + __INT_MAX__) >= 2)
    __builtin_unreachable ();
  int d = p + b;
  int c = p + a;
  if (c - d != __INT_MAX__)
    __builtin_abort ();
}

void
bar (unsigned p, unsigned a)
{
  unsigned q = p + 7;
  if (a - (1U + __INT_MAX__) >= 2)
    __builtin_unreachable ();
  int c = p;
  int d = p + a;
  if (c - d != -__INT_MAX__ - 1)
    __builtin_abort ();
}

int
main ()
{
  foo (-1U, 1U + __INT_MAX__, 1U);
  bar (-1U, 1U + __INT_MAX__);
  return 0;
}
