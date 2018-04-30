// PR c++/82159
// { dg-do compile }
// { dg-options "" }

template<int N>
struct S
{
  ~S () {}
  template<int M> S<M> foo () { return S<M> (); }
  unsigned char data[N];
};

int
main ()
{
  S<16> d;
  S<0> t = d.foo<0> ();
}
