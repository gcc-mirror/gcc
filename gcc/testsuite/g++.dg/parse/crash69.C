// PR c++/84605

struct b {
  int x(((struct b {})));  // { dg-error "expected|redefinition" }
};

struct c {
  struct d {
    int x(((struct c {})));  // { dg-error "expected|redefinition" }
  };
};
