// PR c++/84663

struct S {
  typedef S T[8];
  int f : -1ULL; // { dg-warning "exceeds its type" }
  S () { struct { T d; } e[]; } // { dg-error "size" }
};
