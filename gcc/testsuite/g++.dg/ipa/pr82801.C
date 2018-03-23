// PR ipa/82801
// { dg-do compile }
// { dg-options "-O2 -Wno-attributes" }

template<int>
struct A { A () {} };
struct B { double foo () const; };

__attribute__((always_inline, flatten))
double B::foo () const
{
  A<1> v;
  return 0.0;
}

int
main ()
{
  return 0;
}
