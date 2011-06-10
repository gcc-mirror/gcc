// { dg-options -std=c++98 }

template <int I>
void f();			// { dg-message "note" }

void g() { f<(3, 2)>(); } // { dg-error "" }
