// { dg-options "-fabi-version=0" }

struct A {
  template <typename T> int f ();
  int operator+();
  operator int ();
  template <typename T> 
  int operator-();
};

typedef int (A::*P)();

template <P> struct S {};

template <typename T> void g (S<&T::template f<int> >) {}
template <typename T> void g (S<&T::operator+ >) {}
template <typename T> void g (S<&T::operator int>) {}
template <typename T> void g (S<&T::template operator- <double> >) {}

template void g<A> (S<&A::f<int> >);
template void g<A> (S<&A::operator+>);
template void g<A> (S<&A::operator int>);
template void g<A> (S<&A::operator-<double> >);

// { dg-final { scan-assembler _Z1gI1AEv1SIXadsrT_1fIiEEE } }
// { dg-final { scan-assembler _Z1gI1AEv1SIXadsrT_plEE } }
// { dg-final { scan-assembler _Z1gI1AEv1SIXadsrT_cviEE } }
// { dg-final { scan-assembler _Z1gI1AEv1SIXadsrT_miIdEEE } }
