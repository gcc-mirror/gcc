// Make sure we don't mistakenly mark f as DECL_COMDAT.
// { dg-final { scan-assembler "_Z1fv" } }

void f();

template <class T> struct A
{
  friend void f();
};

A<int> a;

void f() { }
