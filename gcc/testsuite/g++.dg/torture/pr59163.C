// PR target/59163
// { dg-do run }

struct A { float a[4]; };
struct B { int b; A a; };

__attribute__((noinline, noclone)) void
bar (A &a)
{
  if (a.a[0] != 36.0f || a.a[1] != 42.0f || a.a[2] != 48.0f || a.a[3] != 54.0f)
    __builtin_abort ();
}

__attribute__((noinline, noclone)) void
foo (A &a)
{
  int i;
  A c = a;
  for (i = 0; i < 4; i++)
    c.a[i] *= 6.0f;
  a = c;
  bar (a);
}

int
main ()
{
  B b = { 5, { 6, 7, 8, 9 } };
  foo (b.a);
}
