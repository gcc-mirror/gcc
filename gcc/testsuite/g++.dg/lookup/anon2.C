// { dg-do compile }
// { dg-options "" }

// Make sure we don't issue a diagnostic if a type with no linkage is used
// to declare a a variable that has linkage if that variable is defined.

struct { int i; } a;

void foo() { a.i; }
