// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/19395

struct A {
  typedef int ::X; // { dg-error "17:typedef name may not be a nested-name-specifier" }
};


