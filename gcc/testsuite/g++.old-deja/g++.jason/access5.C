// Simple testcase for access control.
// Build don't link:

class A {
 protected:
  void f ();
};

class B : public A { };
class C : public B {
 void f () { B::f(); }
};
