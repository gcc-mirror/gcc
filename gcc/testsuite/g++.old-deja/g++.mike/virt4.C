// { dg-do run  }
// { dg-options "" }

void  Foo () {}

class B {
public:
  virtual void foo() = 0;
};

class D: virtual public B {
public:
  void foo() { Foo(); }
};

class D1: public D {};

class D2: public D {};

class D1_2: public D1, public D2 {
public:
  void foo() { D1::foo(); D2::foo(); }
};

main() {
  D1_2 h;
  h.foo();
}
