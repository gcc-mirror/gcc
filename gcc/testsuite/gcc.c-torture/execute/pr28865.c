struct A { int a; char b[]; };
union B { struct A a; char b[sizeof (struct A) + 31]; };
union B b = { { 1, "123456789012345678901234567890" } };
union B c = { { 2, "123456789012345678901234567890" } };

__attribute__((noinline, noclone)) void
foo (int *x[2])
{
  x[0] = &b.a.a;
  x[1] = &c.a.a;
}

int
main ()
{
  int *x[2];
  foo (x);
  if (*x[0] != 1 || *x[1] != 2)
    __builtin_abort ();
  return 0;
}
