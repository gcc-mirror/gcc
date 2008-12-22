struct A { int i, j; char pad[512]; } a;

int __attribute__((noinline))
foo (void)
{
  __builtin_memset (&a, 0x26, sizeof a);
  return a.i;
}

void __attribute__((noinline))
bar (void)
{
  __builtin_memset (&a, 0x36, sizeof a);
  a.i = 0x36363636;
  a.j = 0x36373636;
}

int
main (void)
{
  int i;
  if (sizeof (int) != 4 || __CHAR_BIT__ != 8)
    return 0;

  if (foo () != 0x26262626)
    __builtin_abort ();
  for (i = 0; i < sizeof a; i++)
    if (((char *)&a)[i] != 0x26)
      __builtin_abort ();

  bar ();
  if (a.j != 0x36373636)
    __builtin_abort ();
  a.j = 0x36363636;
  for (i = 0; i < sizeof a; i++)
    if (((char *)&a)[i] != 0x36)
      __builtin_abort ();
  return 0;
}
