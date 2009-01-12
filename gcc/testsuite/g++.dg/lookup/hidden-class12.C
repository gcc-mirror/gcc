// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/36019
// { dg-do compile }

struct F {
  static const int x = 0;
};

struct A {
  template <typename A>
  static int f ()
  {
    return A::x;
  }
};


int
main ()
{
  int i = A::f<F> ();
  return i;
}

