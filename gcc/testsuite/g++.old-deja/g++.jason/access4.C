// Simple testcase for access control.
// Build don't link:

class A {
protected:
  static int i;
};

class B: public A { };

class C: public B {
public:
  void g () { B b; b.i; }
};
