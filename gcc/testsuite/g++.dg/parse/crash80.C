// PR c++/116740
// { dg-do "compile" }

class K {
  int a(struct b; // { dg-error "expected '\\)'" }
};
struct b {};
