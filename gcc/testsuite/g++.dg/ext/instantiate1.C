// Test that 'extern template' suppresses instantiations.
// { dg-do link }
// { dg-options "" }

template <class T> void f (T) { }
extern template void f (int);

template <class T> struct A {
  void f ();
};
template <class T> void A<T>::f () { }
extern template struct A<int>;

// { dg-error "void f<int>\\(int\\)" "suppressing f<int>" { target *-*-* } "0" }
void test_f_int () { f(42); } 

// { dg-error "A<int>::f\\(\\)" "suppressing A<int>" { target *-*-* } "0" }
void test_A_int_f () { A<int> a; a.f (); }

// { dg-bogus "void f<double>\\(double\\)" "f<double>" { target *-*-* } "0" }
void test_f_double () { f (2.0); }

// { dg-bogus "A<double>::f\\(\\)" "A<double>" { target *-*-* } "0" }
void test_A_double_f () { A<double> b; b.f (); }

int main ()
{
  test_f_int ();
  test_A_int_f ();
  test_f_double ();
  test_A_double_f ();
}
