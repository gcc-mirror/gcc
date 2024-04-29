// { dg-additional-options "-fmodules-ts" }

// 'import X' does not correctly notice that S has already been declared.
struct S {};  // { dg-message "previously declared" "" { xfail *-*-* } }
template <typename> struct T {};  // { dg-message "previously declared" }
void f() {}  // { dg-message "previously declared" }
template <typename T> void g() {}  // { dg-message "previously declared" }

import X;
A<double> a2;  // { dg-message "required from here" }
B<double> b2;  // { dg-message "required from here" }

// specifically, S and T are defined in M, not X, despite the instantiation being in X
// { dg-error "conflicting declaration \[^\n\r\]* S@M" "" { xfail *-*-* } 0 }
// { dg-error "conflicting declaration \[^\n\r\]* T@M" "" { target *-*-* } 0 }
// and similarly for f and g
// { dg-error "conflicting declaration \[^\n\r\]* f@M" "" { target *-*-* } 0 }
// { dg-error "conflicting declaration \[^\n\r\]* g@M" "" { target *-*-* } 0 }
