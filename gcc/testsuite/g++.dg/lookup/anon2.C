// { dg-do compile }
// { dg-options "" }

class { int i; } a; // { dg-error "private" }
void foo() { a.i; } // { dg-error "context" }

