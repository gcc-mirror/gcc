// PR c++/59255
// { dg-options "-O2 -std=c++11" }

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
