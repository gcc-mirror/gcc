// { dg-do run  }
// prms-id: 4068

struct A {
  A();
  typedef void (A::*F)();
  void d();
  void foo() { }
  F& f() { return f_; }
  F f_;
};

A::A() : f_(&A::foo) {
}

void A::d() {
  (this->*(f()))();
}

int main() {
  A a;
  a.d();
}
