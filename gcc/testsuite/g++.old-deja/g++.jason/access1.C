// Bug: access declarations are broken.
// Build don't link:

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
  b.foo ();			// gets bogus error - 
}
