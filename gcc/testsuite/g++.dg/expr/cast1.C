struct S; // { dg-message "forward" } 

void f(S* p) { ((S) (*p)); } // { dg-error "" }
