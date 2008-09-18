// { dg-options "-fshow-column -ansi -pedantic-errors -Wno-long-long" }
// PR c++/15786

struct A {
  void foo(bar* p); /* { dg-error "12:'bar' has not been declared" } */
};
