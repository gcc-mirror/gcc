template <int I>
void f();			// { dg-message "note" }

void g() { f<(3, 2)>(); } // { dg-error "" }
// { dg-message "candidate" "candidate note" { target *-*-* } 4 }
