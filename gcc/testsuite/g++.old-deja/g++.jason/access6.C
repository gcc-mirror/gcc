// Simple testcase for access control.
// Build don't link:

class A {
 public:
  void f ();
};

class B: protected A { };
class C: protected B {
  void g() { f (); }
};
