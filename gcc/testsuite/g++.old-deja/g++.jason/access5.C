// { dg-do assemble  }
// Simple testcase for access control.

class A {
 protected:
  void f ();
};

class B : public A { };
class C : public B {
 void f () { B::f(); }
};
