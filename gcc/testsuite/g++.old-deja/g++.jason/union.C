// { dg-do assemble  }
// Bug: g++ doesn't insert anon union members into class scope.
// Breaks groff.

struct A {
  union {
    int i;
  };

  void foo () { i = 1; }	// { dg-bogus "" } 
};
