struct S; // { dg-error "forward" } 

void f(S* p) { ((S) (*p)); } // { dg-error "" }
