// Test that 'static template' instantiates statics.
// Special g++ Options: -g -fno-implicit-templates

// Ignore the 'ld returned 1' message from collect2.
// excess errors test - XFAIL *-*-*

template <class T> struct A {
  static T t;
};
template <class T> T A<T>::t = 0;
static template struct A<int>;

// These functions must be defined in a single line, so that, even if
// constants or pointers are placed in the code section (for example,
// on the SH), we still get the same line numbers.

void test_int() { A<int>::t = 42; } // gets bogus error

void test_char() { A<char>::t = 42; } // ERROR - not instantiated XFAIL *-*-irix* *-*-hpux*
// Irix's default linker does not produce line numbers so XFAIL it.
// Similarly for HP's linker

int main ()
{
  test_int ();
  test_char ();
}
