/* { dg-options "-O"  } */
/* { dg-do run } */

static __attribute__((noipa)) int foo (void)
{
  return 1;
}

int main (void)
{
  struct S { int a; int b; };
  struct T { struct S s; };

  struct T t = { { 0, 0 } };
  struct T u;

  __attribute__((noinline)) void bar (void)
  {
    if (foo ())
      {
	u = t;
        /* OK with u.s.a = 0; */
      }
  }

  u.s.a = 1;

  bar ();

  if (u.s.a != 0)
    __builtin_abort ();

  return 0;
}
