// PR c++/92992
// { dg-do run { target c++11 } }

int a;

void
bar (int, ...)
{
}

decltype (nullptr)
baz ()
{
  a++;
  return nullptr;
}

int
main ()
{
  bar (0, baz ());
  if (a != 1)
    __builtin_abort ();
}
