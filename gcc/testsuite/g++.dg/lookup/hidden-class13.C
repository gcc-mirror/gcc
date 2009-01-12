// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/36019
// { dg-do compile }

struct F {
  static const int x = 0;
};

struct B {
  template <typename B>
  struct C
  {
    static int f ()
    {
      return B::x;
    }
  };
};

int
main ()
{
  int j = B::C<F>::f ();
  return 0;
}
