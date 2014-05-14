//PR c++/28258

struct A			// { dg-message "note" }
{
  A(void x); // { dg-error "invalid use|incomplete type|candidates" }
  // { dg-message "" "match candidate text" { target *-*-* } 5 }
};

struct B : A {}; // { dg-error "no matching function for call|deleted" }
 
B b; // { dg-message "synthesized method|deleted" }
