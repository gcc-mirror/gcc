// Test that overloading on 'this' quals works with class using-declarations.

// { dg-do link }

struct A {
  void f() const;
  void f() {}
  void g() const {}
  void g();
  void h() const;
  void h();
  void i() const;
  void i() {}
};

struct B: private A {
  using A::f;
  using A::g;
  void h () const {}
  using A::h;
  void i () const {}
  using A::i;
};

int main()
{
  B b1;
  const B b2 = B();

  b1.f ();
  b2.g ();
  b2.h ();
  b1.i ();
}
