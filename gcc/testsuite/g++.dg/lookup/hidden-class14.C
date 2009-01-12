// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/36019
// { dg-do compile }

struct F {
  static const int x = 0;
  typedef int A;
};

struct A {
  template <typename A>
  struct G : public F 
  {
    static const A i = 0;
  };
};

int
main ()
{
  return A::G<F>::i ;
}

