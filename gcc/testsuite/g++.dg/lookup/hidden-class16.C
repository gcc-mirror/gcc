// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/36019
// { dg-do compile }

struct F {
  static const int y = 0;
};

struct A {
  static const int x = 0;
};

struct B : public A {
  template <typename A>
  static int f ()
  {
    return A::x; // { dg-error "'x' is not a member of 'F'" }
  }
};

int
main ()
{
  int j = B::f<F> ();
  return 0;
}

