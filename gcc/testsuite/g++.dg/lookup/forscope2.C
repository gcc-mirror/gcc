// { dg-do compile }

struct S {
  void foo() {
     for (_ptr; ;) {}  // { dg-error "not declared" }
     _ptr = 0;
    }
};

// It's OK to error or not on line 6.
// { dg-prune-output ":6:" }
