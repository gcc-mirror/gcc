// { dg-do assemble  }
// Bug: access declarations are broken.

class A {
public:
  void foo ();
};

class B: private A {
public:
  A::foo;
};

void foo() {
  B b;
  b.foo ();			// { dg-bogus "" } 
}
