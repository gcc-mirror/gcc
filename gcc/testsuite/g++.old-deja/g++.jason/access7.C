// Simple testcase for access control.
// Build don't link:

class A {
 protected:
  static void f ();
};

class B: public A {};
class C: public A {};
class D: public C, public B {
  void g () { A::f(); }		// gets bogus error - wrongly ambiguous static member call
};
