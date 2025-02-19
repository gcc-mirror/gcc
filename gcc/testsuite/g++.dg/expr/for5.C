// PR c++/86769
// { dg-do run }

int g;

struct X {
  X () { g++; }
  ~X () { g--; }
  operator bool () { return g == 0; }
};

void
foo ()
{
  if (g <= 0)
    __builtin_abort ();
}

void
bar ()
{
  if (g)
    __builtin_abort ();
}

int
main ()
{
  for (int i = 0; i < 1; ++i, bar ())
    {
      X x = X ();
      foo ();
    }
}
