/* PR tree-optimization/93945 */

union U { char a[8]; struct S { unsigned int b : 8, c : 13, d : 11; } e; } u;

__attribute__((noipa)) int
foo (void)
{
  __builtin_memset (&u.a, 0xf4, sizeof (u.a));
  return u.e.c;
}

__attribute__((noipa)) int
bar (void)
{
  asm volatile ("" : : "g" (&u) : "memory");
  return u.e.c;
}

__attribute__((noipa)) int
baz (void)
{
  __builtin_memset (&u.a, 0xf4, sizeof (u.a));
  return u.e.d;
}

__attribute__((noipa)) int
qux (void)
{
  asm volatile ("" : : "g" (&u) : "memory");
  return u.e.d;
}

int
main ()
{
  int a = foo ();
  int b = bar ();
  if (a != b)
    __builtin_abort ();
  a = baz ();
  b = qux ();
  if (a != b)
    __builtin_abort ();
  return 0;
}
