// PR c++/5748
// This testcase ICEd because used flag from the anon union variables
// was not propagated back to the anon union itself, causing addressof
// not to be replaced with stack slot.
// { dg-do compile }
// { dg-options "-O2" }

struct A {
  A ();
  ~A ();
  int foo ();
  int bar (void *x, int y);
};

int A::foo()
{
  union {
    int a;
    int b;
  };

  if (bar (&a, sizeof (int)) != 32)
    return 16;
  return 0;
}
