// { dg-do assemble  }

// Based on a testcase by Ruslan Shevchenko <Ruslan@Shevchenko.Kiev.UA>

struct B {
  virtual ~B();
};

struct D : public B {
};

void foo() {
  B x;
  dynamic_cast<D*>(&x); // { dg-warning "3:.dynamic_cast" } will never succeed
  B* p = &x;
  dynamic_cast<D*>(p);
}
