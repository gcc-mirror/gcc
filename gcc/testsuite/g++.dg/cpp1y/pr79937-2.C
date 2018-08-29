// PR c++/79937
// { dg-do run { target c++14 } }

struct C {};

struct X {
  unsigned i;
  unsigned n = i;
  unsigned m = i;
};

C
bar (X x)
{
  if (x.i != 1 || x.n != 2 || x.m != 1)
    __builtin_abort ();
  return {};
}

int
main ()
{
  C c = bar (X {1, X {2}.n});
}
