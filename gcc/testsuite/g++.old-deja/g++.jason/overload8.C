// Bug: g++ fails to catch the ambiguity below.
// Build don't link:

struct A {
  operator int () { return 1; };
  operator int &() { return 1; }; // ERROR - 
};
