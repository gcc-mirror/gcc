// Test that we save declspecs before a class defn properly.
// Build don't link:

static volatile union {
  void f() { }
} u;
