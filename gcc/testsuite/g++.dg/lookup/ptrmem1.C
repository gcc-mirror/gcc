struct A {
  virtual void f ();
};

struct B : public A {
};

struct C : public A {
};

struct D : public B, C {
  virtual void f (); 
};

void (D::*p)() = &D::f;
