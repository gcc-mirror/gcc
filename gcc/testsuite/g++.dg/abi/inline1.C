struct S {
  S() {}
  virtual void g() {}
};

// { dg-final { scan-assembler-not "_ZTV1S" } }
