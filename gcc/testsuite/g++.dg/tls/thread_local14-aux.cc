// PR c++/109164

extern bool baz ();

int
qux ()
{
  return 42;
}

extern thread_local const int t = qux ();

bool
bar (int x)
{
  if (x != 42)
    __builtin_abort ();
  return false;
}

int
main ()
{
  if (baz ())
    __builtin_abort ();
}
