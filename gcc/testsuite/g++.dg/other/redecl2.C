// PR c++/17816
// We failed to report duplicate definitions of pure virtual ns.

// { dg-do compile }

struct S {
  virtual int foo() = 0;
};
 
int S::foo() { return 0; } // { dg-message "defined here" }
int S::foo() { return 0; } // { dg-error "redefinition" }
