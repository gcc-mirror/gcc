// Test that 'extern template' suppresses instantiations.
// { dg-do compile }
// { dg-options "" }

template <class T> void f (T) { }
extern template void f (int);

template <class T> struct A {
  void f ();
};
template <class T> void A<T>::f () { }
extern template struct A<int>;

// { dg-final { scan-assembler-not "\n_?_Z1fIiEvT_(:|\n|\t)" } }
void test_f_int () { f(42); } 

// { dg-final { scan-assembler-not "\n_?_ZN1AIiE1fEv(:|\n|\t)" } }
void test_A_int_f () { A<int> a; a.f (); }

// { dg-final { scan-assembler "\n_?_Z1fIdEvT_(:|\n|\t)" } }
void test_f_double () { f (2.0); }

// { dg-final { scan-assembler "\n_?_ZN1AIdE1fEv(:|\n|\t)" } }
void test_A_double_f () { A<double> b; b.f (); }
