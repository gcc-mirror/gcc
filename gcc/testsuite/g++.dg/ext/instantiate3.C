// Test that 'inline template' instantiates the vtable.
// { dg-do compile }
// { dg-options "-O -fno-implicit-templates" }

template <class T> struct A {
  virtual void f () { }
};
inline template struct A<int>;

// { dg-final { scan-assembler "\n_?_ZTV1AIiE(:|\n|\t)" } }
A<int> a;

// { dg-final { scan-assembler-not "\n_?_ZTV1AIcE(:|\n|\t)" } }
A<char> b;
