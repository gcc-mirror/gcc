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

// These functions must be defined in a single line, so that, even if
// constants or pointers are placed in the code section (for example,
// on the SH), we still get the same line numbers.

void test_f_int () { f(42); } // ERROR - not instantiated

void test_A_int_f () { A<int> a; a.f (); } // ERROR - not instantiated

void test_f_double () { f (2.0); } // gets bogus error

void test_A_double_f () { A<double> b; b.f (); } // gets bogus error

int main ()
{
  test_f_int ();
  test_A_int_f ();
  test_f_double ();
  test_A_double_f ();
}
