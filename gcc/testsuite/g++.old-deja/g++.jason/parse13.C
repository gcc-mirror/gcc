// Build don't link:

struct A { 
  struct B {}; 
  struct C;
};

struct A :: C : A :: B {}; // gets bogus error - parse error before `:'
