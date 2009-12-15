// { dg-do assemble  }
// GROUPS passed niklas ellipsis
typedef void (*T) (...);
void f ();
struct S { void g (T); void h() { g(f); } };// { dg-error "match" "match" } 
// { dg-message "candidate is" "note" { target *-*-* } 5 }
