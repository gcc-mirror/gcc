namespace {
  struct A
  {
    virtual void f();
  };

  void A::f() { }
}

void g() { throw A(); }
