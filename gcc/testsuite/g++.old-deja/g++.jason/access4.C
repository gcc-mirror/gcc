// { dg-do assemble  }
// Simple testcase for access control.

class A {
protected:
  static int i;
};

class B: public A { };

class C: public B {
public:
  void g () { B b; b.i; }
};
