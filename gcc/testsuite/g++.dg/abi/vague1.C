// Test that we don't emit unneeded copies of static data member template
// instantiations.

// Disable debug info so we don't get confused by the symbol name there.
// The test fails on hppa*-*-hpux* because the symbol _ZN1AIiE1tE is imported.
// { dg-options "-g0" }
// { dg-final { if { [istarget hppa*-*-hpux*] } { return } } }
// { dg-final { scan-assembler-not "_ZN1AIiE1tE" } }

template <class T> struct A {
  static const T t = 0;
};

template <class T> const T A<T>::t;

int i;
int main ()
{
  i = A<int>::t;		// Should just use the value
}
