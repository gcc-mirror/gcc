template <int I>
void f();			// { dg-message "candidate" }

void g() { f<(3, 2)>(); } // { dg-error "" }
