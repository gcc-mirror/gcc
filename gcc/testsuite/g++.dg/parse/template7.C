template <int I>
void f();

void g() { f<(3, 2)>(); } // { dg-error "" }
