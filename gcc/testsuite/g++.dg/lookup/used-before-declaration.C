// Copyroght (C) 2003 Free Software Foundation
// Origin: PR/12832, Jonathan Wakely <redi@gcc.gnu.org>

void f() { g(); }               // { dg-error "undeclared" "" }
void g() { }                    // { dg-error "used" "" }
