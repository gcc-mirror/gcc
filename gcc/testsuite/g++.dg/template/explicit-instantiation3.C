// { dg-do compile }
// Origin: <sebor at roguewave dot com>
// c++/8266: Explicit instantiation of a template outside its namespace is
//  broken

namespace N
{
  template <class T> T foo (T)
  { return T (); }

  struct A
  {
    template <int N>
    struct B {};
  };

  template <int M>
  struct C {};

  template double foo(double);
  template float  foo<float>(float);
  template struct A::B<0>;
  template struct C<0>;
}

template int    N::foo(int);
template char   N::foo<char>(char);
template struct N::A::B<1>;
template struct N::C<1>;


