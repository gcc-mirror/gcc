// Simple testcase for access control.
// Build don't link:

class A {
 protected:
  void f ();
};

class B : public A {
 public:
  void f () { A::f(); }
};
