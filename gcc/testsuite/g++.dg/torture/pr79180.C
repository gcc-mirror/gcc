// { dg-do run }
// { dg-options "-Wall -std=c++11" }

void
foo (int a)
{
  if (a != 127)
    __builtin_abort ();
}

template <typename... Args>
void
bar (Args &&... args)
{
  [&]() { [&]() { foo (args...); } (); } ();
}

int
main ()
{
  int x = 127;
  bar (x);
}
