// { dg-do assemble  }
// GROUPS passed niklas ellipsis
void f ();
void g1 (void (*) (...)); void h1 () { g1 (f); }// { dg-error "invalid conversion" }
struct S { void g2 (void (*) (...)); void h2 () { g2 (f); } };// { dg-error "match" "match" } 
// { dg-message "candidate|S::g2|no known conversion" "match candidate text" { target *-*-* } 5 }
