// { dg-do compile }
// { dg-options "" }

// Make sure we issue a diagnostic if a type with no linkage is used
// to declare a a variable that has linkage.

struct { int i; } a; // { dg-warning "anonymous type" }

void foo() { a.i; }
