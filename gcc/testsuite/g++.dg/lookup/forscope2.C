// { dg-do compile }

struct S {
  void foo() {
     for (_ptr; ;) {}  // { dg-error "not declared" }
     _ptr = 0;
    }
};

