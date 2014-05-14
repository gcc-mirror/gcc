// { dg-do assemble  }
// GROUPS passed niklas ellipsis
typedef void (*T) (...);
void f ();
struct S { void g (T); void h() { g(f); } };// { dg-error "" "match" }
// { dg-message "candidate|S::g|no known conversion" "match candidate text" { target *-*-* } 5 }
