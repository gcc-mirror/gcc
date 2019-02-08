// PR c++/86943
// { dg-do run { target c++14 } }

int c[3];

struct S
{
  S () : s (1234) { c[0]++; }
  S (const S &) { __builtin_abort (); }
  S (S &&x) noexcept { if (x.s != 1234) __builtin_abort (); s = 1234; x.s = 2345; c[1]++; }
  ~S () { if (s != 1234 && s != 2345) __builtin_abort (); c[2]++; }
  int s;
};

using F = S (*) (S);

F
foo ()
{
  return [] (auto val)->S { if (val.s != 1234) __builtin_abort (); return {}; };
}

int
main ()
{
  {
    volatile F f = foo ();
    S s = f ({});
    if (s.s != 1234) __builtin_abort ();
  }
  if (c[0] + c[1] != c[2])
    __builtin_abort ();
}
