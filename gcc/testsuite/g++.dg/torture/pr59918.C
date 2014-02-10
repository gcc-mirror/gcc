/* { dg-do compile } */
struct E {
  ~E();
  virtual void f() const;
};
struct B : E {};
struct G : virtual B {};
struct A {
  virtual ~A();
};
struct J : E {
  void f() const {
    E *p = 0;
    p->f();
  }
};
J h;
struct I : A, G, virtual B {};
