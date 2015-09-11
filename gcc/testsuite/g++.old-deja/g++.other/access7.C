// { dg-do assemble  }
// Test that access control for types and statics works properly
// with nested types.


class A {
  static int I1;		// { dg-message "" } private
  struct B1 { };		// { dg-message "" } private
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
  int i = I1;			// { dg-error "" } within this context
  B1 b1;			// { dg-error "" } within this context
  i = I2;
  B2 b2;
}

void f ()
{
  A::B1 b1;			// { dg-error "" } within this context
  new A::B1;			// { dg-error "" } within this context
  (A::B1) b1;			// { dg-error "" } within this context
}
