int baz (void);
static int
foo (int x)
{
  if (__builtin_expect (x <= 0, 0))
    {
      __builtin_printf ("foo\n");
      __builtin_printf ("foo\n");
      __builtin_printf ("foo\n");
      __builtin_abort ();
    }
  return 6;
}

int a,b,c;

int
bar (int x)
{
  return foo (a) + foo (b) + foo (c);
}
