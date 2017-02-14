/* PR tree-optimization/78720 */

__attribute__((noinline, noclone)) long int
foo (signed char x)
{
  return x < 0 ? 0x80000L : 0L;
}

__attribute__((noinline, noclone)) long int
bar (signed char x)
{
  return x < 0 ? 0x80L : 0L;
}

__attribute__((noinline, noclone)) long int
baz (signed char x)
{
  return x < 0 ? 0x20L : 0L;
}

int
main ()
{
  if (foo (-1) != 0x80000L || bar (-1) != 0x80L || baz (-1) != 0x20L
      || foo (0) != 0L || bar (0) != 0L || baz (0) != 0L
      || foo (31) != 0L || bar (31) != 0L || baz (31) != 0L)
    __builtin_abort ();
  return 0;
}
