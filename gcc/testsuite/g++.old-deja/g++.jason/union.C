// Bug: g++ doesn't insert anon union members into class scope.
// Breaks groff.
// Build don't link:

struct A {
  union {
    int i;
  };

  void foo () { i = 1; }	// gets bogus error - 
};
