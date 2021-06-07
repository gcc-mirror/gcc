// PR middle-end/100898

int a;
int bar (int, ...);

static inline __attribute__((always_inline)) int
foo (...)
{
  while (a)
    return bar (0, __builtin_va_arg_pack ());
  return 0;
}

void
baz (void)
{
  foo ();
}
