// Test that access control for types and statics works properly
// with nested types.

// Build don't link:

class A {
  static int I1;		// ERROR - private
  struct B1 { };		// ERROR - private
public:
  static int I2;
  struct B2 { };
};

class D: public A {
  struct E {
    void f ();
  };
};

void D::E::f ()
{
  int i = I1;			// ERROR - within this context
  B1 b1;			// ERROR - within this context
  i = I2;
  B2 b2;
}

void f ()
{
  A::B1 b1;			// ERROR - within this context
  new A::B1;			// ERROR - within this context
  (A::B1) b1;			// ERROR - within this context
}
