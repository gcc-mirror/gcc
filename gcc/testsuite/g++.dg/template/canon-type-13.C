// Contributed by Dodji Seketeli <dodji@redhat.com>
// { dg-do compile }

template<class T>
struct S0
{
};

template<class T>
struct S1
{
};

template<class T, template<class T>  class A, template<class T>  class B = A>
struct C
{
  B<T> m;
};

void
foo()
{
  C<int, S0> s;
  S0<int> s0;

  s.m = s0;
}
