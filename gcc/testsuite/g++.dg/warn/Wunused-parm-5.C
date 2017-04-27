// PR c++/58362
// { dg-options "-Wunused-parameter" }

void f1 (long s) { }  // { dg-warning "15:unused parameter 's'" }

void f2 (long s, int u) { }  // { dg-warning "15:unused parameter 's'" }
// { dg-warning "22:unused parameter 'u'" "" { target *-*-* } .-1 }

void f3 (long s);
void f3 (long s) { }  // { dg-warning "15:unused parameter 's'" }

void f4 (long s, int u);
void f4 (long s, int u) { }  // { dg-warning "15:unused parameter 's'" }
// { dg-warning "22:unused parameter 'u'" "" { target *-*-* } .-1 }
