// Test that 'static template' instantiates statics.
// Special g++ Options: -g -fno-implicit-templates

// Ignore the 'ld returned 1' message from collect2.
// excess errors test - XFAIL *-*-*

template <class T> struct A {
  static T t;
};
template <class T> T A<T>::t = 0;
static template struct A<int>;

int main ()
{
  A<int>::t = 42;		// gets bogus error
  A<char>::t = 42;		// ERROR - not instantiated XFAIL mips*-*-*
				// Irix's default linker does not
				// produce line numbers so XFAIL it.
}
