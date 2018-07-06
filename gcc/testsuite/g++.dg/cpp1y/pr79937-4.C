// PR c++/79937
// { dg-do run { target c++14 } }

struct X {
  unsigned i;
  unsigned n = i;
};

X
bar (X x)
{
  return x;
}

struct Y
{
  static Y bar (Y y) { return y; }
  unsigned i;
  unsigned n = bar (Y{2,i}).n;
};

int
main ()
{
  X x { 1, bar (X{2}).n };
  if (x.n != 2)
    __builtin_abort ();

  Y y { 1 };
  if (y.n != 1)
    __builtin_abort ();
}
