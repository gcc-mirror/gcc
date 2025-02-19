// PR c++/117778
// { dg-do run { target c++20 } }

int
f (auto fp())
{
  return fp ();
}

int
g (auto fp() -> auto)
{
  return fp ();
}

int
h (auto (*fp)() -> auto)
{
  return fp ();
}

auto
fa (auto fp()) -> auto
{
  return fp ();
}

auto
ga (auto fp() -> auto) -> auto
{
  return fp ();
}

auto
ha (auto (*fp)() -> auto) -> auto
{
  return fp ();
}

int bar() { return 42; }

int
main ()
{
  if (f (bar) != 42 || g (bar) != 42 || h (bar) != 42)
    __builtin_abort ();
  if (fa (bar) != 42 || ga (bar) != 42 || ha (bar) != 42)
    __builtin_abort ();
}
