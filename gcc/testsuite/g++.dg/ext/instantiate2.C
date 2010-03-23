// Test that 'static template' instantiates statics.
// { dg-do compile }
// { dg-options "-fno-implicit-templates" }

template <class T> struct A {
  static T t;
};
template <class T> T A<T>::t = 0;
static template struct A<int>;

// { dg-final { scan-assembler "\n_?_ZN1AIiE1tE(:|\n|\t)" { target { ! *-apple-darwin* } } } }
// { dg-final { scan-assembler ".zerofill __DATA, __bss2, __ZN1AIiE1tE" { target *-apple-darwin* } } }
void test_int() { A<int>::t = 42; }

// { dg-final { scan-assembler-not "\n_?_ZN1AIcE1tE(:|\n|\t)" } }
void test_char() { A<char>::t = 42; }
