// { dg-do assemble  }

template <char* c> struct B { B() { } };
B<0> bnull; // { dg-error "" } could not convert template argument
