// PR c++/19238
// Test that hidden visibility on an inline function is inherited by static
// local variables and local classes.

// { dg-do compile { target i?86-*-linux* x86_64-*-linux* powerpc*-*-linux* } }
// { dg-final { scan-assembler "hidden\[ \t\]*_Z1fv" } }
// { dg-final { scan-assembler "hidden\[ \t\]*_ZZ1fvE1i" } }
// { dg-final { scan-assembler "hidden\[ \t\]*_ZZ1fvEN1A1fEv" } }

__attribute ((visibility ("hidden"))) inline int
f()
{
  static int i = 2;
  struct A
  {
    void f () { }
  } a;
  a.f();
  return i;
}

int main()
{
  f();
}
