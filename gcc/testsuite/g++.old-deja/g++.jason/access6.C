// { dg-do assemble  }
// Simple testcase for access control.

class A {
 public:
  void f ();
};

class B: protected A { };
class C: protected B {
  void g() { f (); }
};
