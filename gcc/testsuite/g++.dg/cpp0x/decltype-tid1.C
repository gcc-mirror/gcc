// PR c++/87513
// { dg-do compile { target c++11 } }

struct A { template <long> void foo (); };
template <long t> auto bar () -> decltype (&A::foo<t>);
void foo ()
{
  bar<0> ();
}
