// { dg-do compile }
// { dg-options "-std=c++2a -fcontracts" }

struct Foo {
  virtual int f0(int n) [[ pre: false ]] { return n; }
  virtual int f1(int n) [[ pre: false ]] { return n; }
  virtual int f2(int n) [[ pre: false ]] { return n; }
  virtual int f3(int n) [[ pre: false ]] { return n; }
  virtual int f4(int n) [[ pre: false ]] { return n; }
  virtual int f5(int n) [[ pre: false ]] { return n; }
  virtual int f6(int n) [[ pre: false ]] { return n; }
  virtual int f7(int n) [[ pre: false ]] { return n; }
  virtual int f8(int n) [[ pre: false ]] { return n; }
  virtual int f9(int n) [[ pre: false ]] { return n; }
  virtual int f10(int n) [[ pre: false ]] { return n; }
  virtual int f11(int n) [[ pre: n > 0 ]] [[ pre: n > 1 ]] { return n; }
  virtual int f12(int n) [[ pre: n > 0 ]] [[ pre: n > 1 ]] { return n; }
};

struct Bar : Foo {
  [[ pre: n > -1 ]] int f0(int n = 0) override { return -n; } // { dg-error "contracts must appertain" }
  int [[ pre: n > -2 ]] f1(int n = 0) override { return -n; } // { dg-error "contracts must appertain" }
  int f2 [[ pre: n > -3 ]] (int n = 0) override { return -n; } // { dg-error "contracts must appertain" }
  int f4([[ pre: n > -4 ]] int n = 0) override { return -n; } // { dg-error "contracts must appertain" }
  int f5(int [[ pre: n > -5 ]] n = 0) override { return -n; } // { dg-error "contracts must appertain" }
  int f6(int n [[ pre: n > -6 ]] = 0) override { return -n; } // { dg-error "contracts must appertain" }
  int f7(int n = [[ pre: n > -7 ]] 0) override { return -n; }
  // { dg-error "expected identifier" "" { target *-*-* } .-1 }
  // { dg-error "expected .\{. before numeric" "" { target *-*-* } .-2 }
  // { dg-error "invalid user-defined conversion" "" { target *-*-* } .-3 }
  // { dg-error "expected .,." "" { target *-*-* } .-4 }
  int f8(int n = 0 [[ pre: n > -8 ]]) override { return -n; }
  // { dg-error "shall only introduce an attribute" "" { target *-*-* } .-1 }
  int f9(int n = 0) [[ pre: n > -9 ]] override { return -n; } // { dg-error "mismatched contract" }

  // The grammar doesn't appear to permit contracts after the virt-specifiers
  // but the parser will happily add these to an attribute list that is not
  // owned by the function declarator.
  int f10(int n = 0) override [[ pre: n > -10 ]] { return -n; } // { dg-error "contracts must appertain" }
  int f11(int n) [[ pre: n > 1 ]] override [[ pre: n > 0 ]] { return -n; } // { dg-error "contracts must appertain" }
  int f12(int n) [[ pre: n > 0 ]] override [[ pre: n > 1 ]] { return -n; } // { dg-error "contracts must appertain" }
};

