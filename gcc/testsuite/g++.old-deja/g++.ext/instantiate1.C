// Test that 'extern template' suppresses instantiations.
// Special g++ Options: -g -O

// Ignore the 'ld returned 1' message from collect2.
// excess errors test - XFAIL *-*-*

template <class T> void f (T) { }
extern template void f (int);

template <class T> struct A {
  void f ();
};
template <class T> void A<T>::f () { }
extern template struct A<int>;

int main ()
{
  f (42);			// ERROR - not instantiated
  A<int> a;
  a.f ();			// ERROR - not instantiated
  f (2.0);			// gets bogus error
  A<double> b;
  b.f ();			// gets bogus error
}
