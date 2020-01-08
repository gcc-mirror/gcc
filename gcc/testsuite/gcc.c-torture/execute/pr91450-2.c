/* PR middle-end/91450 */

__attribute__((noipa)) void
foo (int a, int b)
{
  unsigned long long r;
  if (__builtin_mul_overflow (a, b, &r))
    __builtin_abort ();
  if (r != 0)
    __builtin_abort ();
}

__attribute__((noipa)) void
bar (int a, int b)
{
  unsigned long long r;
  if (a >= 0)
    return;
  if (__builtin_mul_overflow (a, b, &r))
    __builtin_abort ();
  if (r != 0)
    __builtin_abort ();
}

__attribute__((noipa)) void
baz (int a, int b)
{
  unsigned long long r;
  if (b >= 0)
    return;
  if (__builtin_mul_overflow (a, b, &r))
    __builtin_abort ();
  if (r != 0)
    __builtin_abort ();
}

__attribute__((noipa)) void
qux (int a, int b)
{
  unsigned long long r;
  if (a >= 0)
    return;
  if (b < 0)
    return;
  if (__builtin_mul_overflow (a, b, &r))
    __builtin_abort ();
  if (r != 0)
    __builtin_abort ();
}

__attribute__((noipa)) void
quux (int a, int b)
{
  unsigned long long r;
  if (a < 0)
    return;
  if (b >= 0)
    return;
  if (__builtin_mul_overflow (a, b, &r))
    __builtin_abort ();
  if (r != 0)
    __builtin_abort ();
}

int
main ()
{
  foo (-4, 0);
  foo (0, -4);
  foo (0, 0);
  bar (-4, 0);
  baz (0, -4);
  qux (-4, 0);
  quux (0, -4);
  return 0;
}
