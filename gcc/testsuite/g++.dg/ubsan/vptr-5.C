// { dg-do run }
// { dg-options "-fsanitize=vptr" }

struct S
{
  S() : a(0) {}
  ~S() {}
  int a;
  int f() { return 0; }
  virtual int v() { return 0; }
};

struct T : S
{
  T() : b(0) {}
  int b;
  int g() { return 0; }
  virtual int v() { return 1; }
};

T *
foo (S *p)
{
  return (T *) p;
}

int
main ()
{
  if (foo (__null) != __null)
    __builtin_abort ();
}
