// PR c++/109164

struct S { virtual void foo (); int s; };
extern bool baz ();

void
S::foo ()
{
  if (s != 42)
    __builtin_abort ();
}

S s;

S &
qux ()
{
  s.s = 42;
  return s;
}

thread_local S &t = qux ();

bool
bar ()
{
  return false;
}

int
main ()
{
  if (baz ())
    __builtin_abort ();
}
