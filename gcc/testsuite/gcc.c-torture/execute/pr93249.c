/* PR tree-optimization/93249 */

char a[2], b[4], c[6];

void
foo (void)
{
  char d[2] = { 0x00, 0x11 };
  __builtin_strncpy (&b[2], d, 2);
  __builtin_strncpy (&b[1], a, 2);
  if (b[0] || b[1] || b[2] || b[3])
    __builtin_abort ();
}

void
bar (void)
{
  __builtin_strncpy (&b[2], "\0\x11", 2);
  __builtin_strncpy (&b[1], a, 2);
  if (b[0] || b[1] || b[2] || b[3])
    __builtin_abort ();
}

void
baz (void)
{
  __builtin_strncpy (&c[2], "\x11\x11\0\x11", 4);
  __builtin_strncpy (&c[1], a, 2);
  if (c[0] || c[1] || c[2] || c[3] != 0x11 || c[4] || c[5])
    __builtin_abort ();
}

int
main ()
{
  foo ();
  bar ();
  baz ();
  return 0;
}
