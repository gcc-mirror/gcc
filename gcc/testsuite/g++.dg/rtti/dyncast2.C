// PR c++/34364
// { dg-do run }

struct A
{
  virtual ~A () {}
};

struct B : public A
{
  template <typename T> struct C
  {
    static void f (A &a)
    {
      dynamic_cast <B &>(a).g ();
    }
  };

  B () : c (6) {}
  void g () { c++; }
  int c;
};

B b;

int
main (void)
{
  B::C<int>::f (b);
  return b.c != 7;
}
