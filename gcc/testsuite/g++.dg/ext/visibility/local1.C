// PR c++/19238
// Test that hidden visibility on an inline function is inherited by static
// local variables and local classes.

// { dg-require-visibility "" }
// { dg-final { scan-hidden "_Z1fv" } }
// { dg-final { scan-hidden "_ZZ1fvE1i" } }
// { dg-final { scan-hidden "_ZZ1fvEN1A1fEv" } }

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
