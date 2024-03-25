// PR c++/59255
// Explicit { dg-require-effective-target exceptions_enabled } so that dependent tests don't turn UNRESOLVED for '-fno-exceptions'.
// { dg-options "-std=c++11 -O2" }

struct S
{
  __attribute__((noinline, noclone)) ~S () noexcept (true)
  {
    if (fn)
      fn (1);
  }
  void (*fn) (int);
};

__attribute__((noinline, noclone)) void
foo (int x)
{
  if (x != 1)
    throw 1;
}

int
main ()
{
  for (int i = 0; i < 100; i++)
    {
      S s;
      s.fn = foo;
    }
}
