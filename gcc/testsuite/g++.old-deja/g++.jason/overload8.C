// { dg-do assemble  }
// Bug: g++ fails to catch the ambiguity below.

struct A {
  operator int () { return 1; };
  operator int &() { return 1; }; // { dg-error "" } 
};
