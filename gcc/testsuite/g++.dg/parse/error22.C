// PR c++/15786

struct A {
  void foo(bar* p); /* { dg-error "declared" } */
};
