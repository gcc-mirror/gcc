// PRMS Id: 6486
// Make sure that no confused handling of COND_EXPRs and SAVE_EXPRs messes
// with the number of calls to foo.

int c;

struct A {
  void f () {}
  virtual void g () {}
};

A& foo ()
{
  static A a;
  ++c;
  return a;
}

int main ()
{
  void (A::*p)() = &A::f;
  (foo ().*p)();
  p = &A::g;
  (foo ().*p)();

  return 2 - c;
}
