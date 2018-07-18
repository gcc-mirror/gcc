/* PR middle-end/77718 */

char a[64] __attribute__((aligned (8)));

__attribute__((noinline, noclone)) int
foo (void)
{
  return __builtin_memcmp ("bbbbbb", a, 6);
}

__attribute__((noinline, noclone)) int
bar (void)
{
  return __builtin_memcmp (a, "bbbbbb", 6);
}

int
main ()
{
  __builtin_memset (a, 'a', sizeof (a));
  if (((foo () < 0) ^ ('a' > 'b'))
      || ((bar () < 0) ^ ('a' < 'b')))
    __builtin_abort ();
  return 0;
}
