//PR c++/28258

struct A 
{            // { dg-error "" }
  A(void x); // { dg-error "invalid use|incomplete type|candidates" }
};

struct B : A {}; // { dg-error "no matching function for call" }
 
B b; // { dg-error "synthesized method" }
