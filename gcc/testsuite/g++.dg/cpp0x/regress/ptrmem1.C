// PR c++/49298
// { dg-options -std=c++0x }

template <class T, int T::*> struct B { };
template <class T> struct A
{
  int i;
  B<A,&A::i> b;
};
