// Test that 'inline template' instantiates the vtable.
// Special g++ Options: -g -O -fno-implicit-templates

// Ignore the 'ld returned 1' message from collect2.
// excess errors test - XFAIL *-*-*

template <class T> struct A {
  virtual void f () { }
};
inline template struct A<int>;

A<int> a;			// gets bogus error
A<char> b;			// ERROR - not instantiated XFAIL mips*-*-* *-*-hpux*
				// Irix's default linker does not
				// produce line numbers so XFAIL it.
				// Similarly for HPUX.

int main ()
{
}
