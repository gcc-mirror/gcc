// PR c++/13140

struct foo {
  foo();
  void f();
  static int i;
};


namespace bar {
  foo::foo() {} // { dg-error "namespace" }
  void foo::f() {} // { dg-error "namespace" }
  int foo::i; // { dg-error "namespace" } 
}
