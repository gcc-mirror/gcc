// PR middle-end/120629
// { dg-do run }
// { dg-options "-O2 -fprofile-generate -fno-exceptions -fno-rtti" }
// { dg-require-profiling "-fprofile-generate" }

__attribute__((noipa, noreturn, cold)) void
foo (const char *, int, const char *)
{
  __builtin_abort ();
}

struct S
{
  __attribute__((noipa)) void bar (void *);
  static const int a = 8;
  unsigned int b[a + 1];
};

__attribute__((noipa)) unsigned long
baz (void *)
{
  static int x = 8;
  return --x;
}

__attribute__((noipa)) void
S::bar (void *x)
{
  unsigned int c;
  int k = 0;

  do
    {
      ((void) (!(k <= a) ? foo ("foo", 42, __FUNCTION__), 0 : 0));
      c = b[k++] = baz (x);
    }
  while (c);
  while (k <= a)
    b[k++] = 0;
}

int
main ()
{
  struct T { S a; unsigned int b; } s = {};
  s.b = 0x1234;
  s.a.bar (0);
  for (int i = 0; i < 9; ++i)
    if (s.a.b[i] != (i == 8 ? 0 : 7 - i))
      __builtin_abort ();
  if (s.b != 0x1234)
    __builtin_abort ();
}
