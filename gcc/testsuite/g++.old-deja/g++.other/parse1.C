// { dg-do assemble  }
// Test that we save declspecs before a class defn properly.

static volatile union {
  void f() { }
} u;
