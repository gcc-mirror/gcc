int i;

__attribute__((noinline, noclone)) void
bar (char *p)
{
  if (i < 1 || i > 6)
    __builtin_abort ();
  if (__builtin_memcmp (p, "abcdefg", i + 1) != 0)
    __builtin_abort ();
  __builtin_memset (p, ' ', 7);
}

__attribute__((noinline, noclone)) void
foo (char *p, unsigned long l)
{
  if (l < 1 || l > 6)
    return;
  char buf[7];
  __builtin_memcpy (buf, p, l + 1);
  bar (buf);
}

int
main ()
{
  for (i = 0; i < 16; i++)
    foo ("abcdefghijklmnop", i);
  return 0;
}
