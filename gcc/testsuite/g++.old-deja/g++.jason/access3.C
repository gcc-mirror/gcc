// { dg-do assemble  }
// Simple testcase for access control.

class A {
 protected:
  void f ();
};

class B : public A {
 public:
  void f () { A::f(); }
};
