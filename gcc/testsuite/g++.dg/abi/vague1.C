// Test that we don't emit unneeded copies of static data member template
// instantiations.

// Disable debug info so we don't get confused by the symbol name there.
// { dg-options "-g0" }

template <class T> struct A {
  static const T t = 0;
};

template <class T> const T A<T>::t;

int i;
int main ()
{
  i = A<int>::t;		// Should just use the value
}
