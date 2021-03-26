// PR c++/99705
// { dg-do compile { target c++11 } }

template <typename T>
struct C
{
  C () { f (); }
  ~C () {}
  static void f () {}
};

struct X
{
  X ();
  int n = 10;
  C<int> *p = new C<int>[n];
};

X::X ()
{
}
