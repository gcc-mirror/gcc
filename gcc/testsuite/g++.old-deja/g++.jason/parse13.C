// { dg-do assemble  }

struct A { 
  struct B {}; 
  struct C;
};

struct A :: C : A :: B {}; // { dg-bogus "" } parse error before `:'
