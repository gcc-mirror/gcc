// Build don't link:

// Based on a testcase by Ruslan Shevchenko <Ruslan@Shevchenko.Kiev.UA>

struct B {
  virtual ~B();
};

struct D : public B {
};

void foo() {
  B x;
  dynamic_cast<D*>(&x); // WARNING - will never succeed
  B* p = &x;
  dynamic_cast<D*>(p);
}
