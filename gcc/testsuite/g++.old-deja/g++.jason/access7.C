// { dg-do assemble  }
// Simple testcase for access control.

class A {
 protected:
  static void f ();
};

class B: public A {};
class C: public A {};
class D: public C, public B {
  void g () { A::f(); }		// { dg-bogus "" } wrongly ambiguous static member call
};
