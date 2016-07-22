// Copyroght (C) 2003 Free Software Foundation
// Origin: PR/12832, Jonathan Wakely <redi@gcc.gnu.org>

void f() { g(); }               // { dg-error "12:'g' was not declared" "" }
void g() { }
