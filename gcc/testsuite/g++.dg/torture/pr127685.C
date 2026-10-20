// PR c++/127685
// { dg-do run }

struct C {
  bool foo (bool) { return false; }
  bool bar (bool) { return false; }
  bool baz (bool);
};

__attribute__ ((noipa))
bool
C::baz (bool b)
{
  static bool (C::*A[]) (bool) = { &C::foo, &C::bar, };
  return (this->*A[(int) b]) (b);
}

int
main ()
{
  C o;
  o.baz (false);
}
